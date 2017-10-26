package scala.tools.nsc.profile

import java.util.Collections
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.util.concurrent._
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.tools.nsc.{Global, Phase}

sealed trait AsyncHelper {

  def newUnboundedQueueFixedThreadPool
  (nThreads: Int,
   shortId: String, priority : Int = Thread.NORM_PRIORITY) : ThreadPoolExecutor
  def newBoundedQueueFixedThreadPool
  (nThreads: Int, maxQueueSize: Int, rejectHandler: RejectedExecutionHandler,
   shortId: String, priority : Int = Thread.NORM_PRIORITY) : ThreadPoolExecutor

}

object AsyncHelper {
  def apply(global: Global, phase: Phase): AsyncHelper = global.currentRun.profiler match {
    case NoOpProfiler => new BasicAsyncHelper(global, phase)
    case r: RealProfiler => new ProfilingAsyncHelper(global, phase, r)
  }

  private abstract class BaseAsyncHelper(global: Global, phase: Phase) extends AsyncHelper {
    val baseGroup = new ThreadGroup(s"scalac-${phase.name}")
    private def childGroup(name: String) = new ThreadGroup(baseGroup, name)

    protected type ProfileData <: AnyRef

    protected def wrapRunnable(r:Runnable, profileData: ProfileData) : Runnable
    protected class CommonThreadFactory(shortId: String, profileData: ProfileData,
                                      daemon: Boolean = true,
                                      priority: Int) extends ThreadFactory {
      private val group: ThreadGroup = childGroup(shortId)
      private val threadNumber: AtomicInteger = new AtomicInteger(1)
      private val namePrefix = s"${baseGroup.getName}-$shortId-"

      override def newThread(r: Runnable): Thread = {
        val wrapped = wrapRunnable(r, profileData)
        val t: Thread = new Thread(group, wrapped, namePrefix + threadNumber.getAndIncrement, 0)
        if (t.isDaemon != daemon) t.setDaemon(daemon)
        if (t.getPriority != priority) t.setPriority(priority)
        t
      }
    }
  }
  private final class BasicAsyncHelper(global: Global, phase: Phase) extends BaseAsyncHelper(global, phase){

    override def newUnboundedQueueFixedThreadPool(nThreads: Int, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new CommonThreadFactory(shortId, null, priority = priority)
      //like Executors.newFixedThreadPool
      new ThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable], threadFactory)
    }

    override def newBoundedQueueFixedThreadPool(nThreads: Int, maxQueueSize: Int, rejectHandler: RejectedExecutionHandler, shortId: String, priority: Int): ThreadPoolExecutor = {
      val threadFactory = new CommonThreadFactory(shortId, null, priority = priority)
      //like Executors.newFixedThreadPool
      new ThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueueSize), threadFactory, rejectHandler)
    }

    override type ProfileData = Void

    override protected def wrapRunnable(r: Runnable, profileData: ProfileData): Runnable = r
  }

  private class ProfilingAsyncHelper(global: Global, phase: Phase, private val profiler:RealProfiler) extends BaseAsyncHelper(global, phase){

    override def newUnboundedQueueFixedThreadPool(nThreads: Int, shortId: String, priority: Int): ThreadPoolExecutor = {
      val background = profiler.registerBackground(global, phase, shortId)
      val allProfileData = new AllProfileData(background)
      val threadFactory = new CommonThreadFactory(shortId, allProfileData, priority = priority)
      //like Executors.newFixedThreadPool
      new SinglePhaseInstrumentedThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable], threadFactory, new AbortPolicy, allProfileData)
    }

    override def newBoundedQueueFixedThreadPool(nThreads: Int, maxQueueSize: Int, rejectHandler: RejectedExecutionHandler, shortId: String, priority: Int): ThreadPoolExecutor = {
      val background = profiler.registerBackground(global, phase, shortId)
      val allProfileData = new AllProfileData(background)
      val threadFactory = new CommonThreadFactory(shortId, allProfileData, priority = priority)
      //like Executors.newFixedThreadPool
      new SinglePhaseInstrumentedThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueueSize), threadFactory, rejectHandler, allProfileData)
    }

    override protected type ProfileData = AllProfileData

    override protected def wrapRunnable(r: Runnable, profileData: ProfileData): Runnable =
      () => {
        val data = new ThreadProfileData
        profileData.running += data
        localData.set(data)
        try r.run finally profileData.processThreadStats(data)
      }
    class AllProfileData(private val id: profiler.Background) {

      import scala.collection.JavaConverters._

      val running = Collections.newSetFromMap(new ConcurrentHashMap[ThreadProfileData, java.lang.Boolean]).asScala
      private var shuttingDown = false

      private var completedThreadCount = 0

      private var idleNs = 0L
      private var runningNs = 0L

      private var firstStartNs = 0L
      private var lastEndNs = 0L

      private var profileCounters: ProfileCounters = Profiler.emptySnap

      def processThreadStats(data: ThreadProfileData): Unit = {
        val snap = profiler.snapBackgroundThread()
        this.synchronized {

          completedThreadCount += 1
          idleNs += data.idleNs
          runningNs += data.runningNs

          firstStartNs = if (firstStartNs == 0L) data.firstStartNs else Math.min(firstStartNs, data.firstStartNs)
          lastEndNs = if (lastEndNs == 0L) data.lastEndNs else Math.max(lastEndNs, data.lastEndNs)

          profileCounters += snap

          running -= data

          if (shuttingDown && running.isEmpty) publishStatsNow
        }
      }
      def publishStatsWhenDone(): Unit = this.synchronized {
        shuttingDown = true

        if (running isEmpty) publishStatsNow
      }
      def publishStatsNow(): Unit =
          profiler.completeBackground(id, firstStartNs, lastEndNs, completedThreadCount, runningNs, idleNs, profileCounters)
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

    private class SinglePhaseInstrumentedThreadPoolExecutor
    ( corePoolSize: Int, maximumPoolSize: Int, keepAliveTime: Long, unit: TimeUnit,
    workQueue: BlockingQueue[Runnable], threadFactory: ThreadFactory, handler: RejectedExecutionHandler, allProfileData: AllProfileData
    ) extends ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory,handler) {

      override def beforeExecute(t: Thread, r: Runnable): Unit = {
        val data = localData.get
        data.taskCount += 1
        val now = System.nanoTime()

        if (data.firstStartNs == 0) {
          data.firstStartNs = now

        }
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

      override def terminated(): Unit = {
        allProfileData.publishStatsWhenDone()
        super.terminated()
      }
    }
  }
}