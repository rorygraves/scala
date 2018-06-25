package scala.tools.nsc.profile

import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.tools.nsc.{Global, Phase}

sealed trait ThreadPoolFactory {
  def unboundedQueueExecutorServiceforPhase(phase: Phase): ExecutionContextExecutorService
  def afterPhase(phase: Phase): Unit
  def afterRun(): Unit

  def newBoundedQueueFixedThreadPool(
      phase: Phase,
      nThreads: Int,
      maxQueueSize: Int,
      rejectHandler: RejectedExecutionHandler,
      shortId: String,
      priority: Int = Thread.NORM_PRIORITY): ThreadPoolExecutor
}

object ThreadPoolFactory {
  def apply(global: Global): ThreadPoolFactory = {
    global.currentRun.profiler match {
      case NoOpProfiler => new BasicThreadPoolFactory(global)
      case r: RealProfiler => new ProfilingThreadPoolFactory(r, global)
    }
  }

  private abstract class BaseThreadPoolFactory(val global: Global) extends ThreadPoolFactory {
    val defaultThreads = global.settings.YparallelThreads.value
    val runId = global.currentRunId

    val baseGroup = new ThreadGroup(s"scalac-run-$runId")

    private def childGroup(name: String) = new ThreadGroup(baseGroup, name)

    protected class CommonThreadFactory(
        shortId: String,
        daemon: Boolean,
        priority: Int) extends ThreadFactory {
      private val group: ThreadGroup = childGroup(shortId)
      private val threadNumber: AtomicInteger = new AtomicInteger(1)
      private val namePrefix = s"${baseGroup.getName}-$shortId-"

      // Invoked when a new `Worker` is created, see `newThread`
      protected def wrapWorker(worker: Runnable, shortId: String): Runnable = worker
      // Invoked by the `ThreadPoolExecutor` when creating a new worker thread. The argument
      // runnable is the `Worker` (which extends `Runnable`). Its `run` method gets tasks from
      // the thread pool and executes them (on the thread created here).
      override def newThread(worker: Runnable): Thread = {
        val wrapped = wrapWorker(worker, shortId)
        val t: Thread = new Thread(group, wrapped, namePrefix + threadNumber.getAndIncrement, 0)
        if (t.isDaemon != daemon) t.setDaemon(daemon)
        if (t.getPriority != priority) t.setPriority(priority)
        t
      }
    }
  }

  private final class BasicThreadPoolFactory(global: Global) extends BaseThreadPoolFactory(global) {

    private var sharedService = Option.empty[ExecutionContextExecutorService]
    override def unboundedQueueExecutorServiceforPhase(phase: Phase): ExecutionContextExecutorService = {
      sharedService match {
        case None =>
          val javaExecutor = newUnboundedQueueFixedThreadPool(defaultThreads, "worker", Thread.NORM_PRIORITY)
          val exec = ExecutionContext.fromExecutorService(javaExecutor, _ => ())
          sharedService = Some(exec)
          exec
        case Some(exec) => exec
      }
    }

    /**
      * we share the service, so end of phase is a Noop
      * @param phase the phase that is stopping - but ignored
      */
    override def afterPhase(phase: Phase): Unit = ()

    /**
      * after a run we start the shutdown of the threads, but dont wait for then to complete
      * we know that all of the work is done
      */
    override def afterRun(): Unit = {
      sharedService.foreach(_.shutdown)
    }

    private def newUnboundedQueueFixedThreadPool(nThreads: Int, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new CommonThreadFactory(shortId, false, priority)
      //like Executors.newFixedThreadPool
      new ThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable], threadFactory)
    }

    override def newBoundedQueueFixedThreadPool(phase: Phase, nThreads: Int, maxQueueSize: Int, rejectHandler: RejectedExecutionHandler, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new CommonThreadFactory(shortId, false, priority)
      //like Executors.newFixedThreadPool
      new ThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueueSize), threadFactory, rejectHandler)
    }
  }

  private class ProfilingThreadPoolFactory(profiler: RealProfiler, global: Global) extends BaseThreadPoolFactory(global) {

    private var phase = Option.empty[Phase]
    private var sharedService = Option.empty[ExecutionContextExecutorService]
    private val runningThreads = new AtomicInteger()

    override def unboundedQueueExecutorServiceforPhase(phase: Phase): ExecutionContextExecutorService = {
      require(sharedService.isEmpty)
      val javaExecutor = newUnboundedQueueFixedThreadPool(phase, defaultThreads, "worker", Thread.NORM_PRIORITY)
      val exec = ExecutionContext.fromExecutorService(javaExecutor, _ => ())
      sharedService = Some(exec)
      exec
    }

    override def afterPhase(phase: Phase): Unit = {
      sharedService.foreach(_.shutdown())
      sharedService = None
    }

    override def afterRun(): Unit = {
      runningThreads.synchronized {
        var remaining = runningThreads.get()
        while (remaining > 0) {
          global.inform(s"waiting for $remaining threads to complete")
          runningThreads.wait(1000)
          remaining = runningThreads.get()
        }
      }
    }

    private def newUnboundedQueueFixedThreadPool(phase: Phase, nThreads: Int, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new SinglePhaseThreadFactory(shortId, false, priority, phase)
      //like Executors.newFixedThreadPool
      new SinglePhaseInstrumentedThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable], threadFactory, new AbortPolicy)
    }

    override def newBoundedQueueFixedThreadPool(phase: Phase, nThreads: Int, maxQueueSize: Int, rejectHandler: RejectedExecutionHandler, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new SinglePhaseThreadFactory(shortId, false, priority, phase)
      //like Executors.newFixedThreadPool
      new SinglePhaseInstrumentedThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueueSize), threadFactory, rejectHandler)
    }

    class SinglePhaseThreadFactory(shortId: String,
                                   daemon: Boolean,
                                   priority: Int,
                                   phase: Phase) extends CommonThreadFactory(shortId, daemon, priority) {
      override protected def wrapWorker(worker: Runnable, shortId: String): Runnable = () => {
        val data = new ThreadProfileData
        localData.set(data)

        val profileStart = profiler.snapThread(0)
        runningThreads.incrementAndGet()
        try worker.run finally {
          val now = System.nanoTime()

          if (data.firstStartNs != 0)
            data.idleNs += now - data.lastEndNs
          val snap = profiler.snapThread(data.idleNs)
          val threadRange = ProfileRange(profileStart, snap, phase, shortId, data.taskCount, Thread.currentThread())
          profiler.completeBackground(threadRange)
          if (runningThreads.decrementAndGet() == 0) {
            runningThreads.synchronized(runningThreads.notifyAll())
          }
        }
      }
    }

    /**
      * data for thread run. Not threadsafe, only written from a single thread
      */
    final class ThreadProfileData {
      var firstStartNs = 0L
      var taskCount = 0

      var idleNs = 0L
      var runningNs = 0L

      var lastStartNs = 0L
      var lastEndNs = 0L
    }

    val localData = new ThreadLocal[ThreadProfileData]

    private class SinglePhaseInstrumentedThreadPoolExecutor(
        corePoolSize: Int, maximumPoolSize: Int, keepAliveTime: Long, unit: TimeUnit,
        workQueue: BlockingQueue[Runnable], threadFactory: ThreadFactory, handler: RejectedExecutionHandler)
      extends ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory, handler) {

      override def beforeExecute(t: Thread, r: Runnable): Unit = {
        val data = localData.get
        data.taskCount += 1
        val now = System.nanoTime()

        if (data.firstStartNs == 0) data.firstStartNs = now
        else data.idleNs += now - data.lastEndNs

        data.lastStartNs = now

        super.beforeExecute(t, r)
      }

      override def afterExecute(r: Runnable, t: Throwable): Unit = {
        val now = System.nanoTime()
        val data = localData.get

        data.lastEndNs = now
        data.runningNs += now - data.lastStartNs

        super.afterExecute(r, t)
      }
    }
  }
}