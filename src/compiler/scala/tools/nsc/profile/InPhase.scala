package scala.tools.nsc.profile

import java.lang.ThreadLocal
import java.util
import java.util.Collections
import java.util.concurrent._
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.tools.nsc.{Global, Phase}
object InPhase {
  val idGen = new AtomicInteger
}
/**
  * A wrapper to allow actions to be associated to a Phase. This aids profiling, particularly where a actions occur in
  * multiple threads, or out of order
  *
  * When you are running a compilation task that involved some activity on a background thread
  * (not the one running [[Global.compileUnits]]) the profiler is not aware of that thread and so cannot account
  * for the activity.
  *
  * By wrapping the activity in this class or one of it children the profiler (if enabled) is informed
  * and the statistics can be gathered
  *
  * No InPhase should run concurrently with another InPhase on the same thread - the statistics dont cope with nesting
  */
sealed abstract class InPhase(global: Global, val phase:Phase, val comment:String) {

  private[profile] final val id = InPhase.idGen.incrementAndGet()
  private[profile] final val profiler = global.currentRun.profiler
  private[profile] final var idleNs = 0L
  profiler.registerInPhase(this)

  @inline protected [profile] def doAction[T] (fn : => T) : T = {
    val before = profiler.beforeInPhase(this)
    try fn
    finally profiler.afterInPhase(this, before, idleNs)
  }

  /**
    * If the compilation activity has some idle time waiting on a future, then this can be recorded by
    * using this method to perform the wait for you. This allow the profiler to distinguish idle time (waiting for some
    * related activity to complete), from for example waiting on I/O
    * @param future the future that you are waiting on
    * @param duration the maximum duration to wait
    */
  def idle(future: Future[_], duration:Duration = Duration.Inf): Unit = {
    if (!future.isCompleted) {
      val start = System.nanoTime()
      try Await.ready(future, duration)
      finally idleNs += (System.nanoTime() - start)
    }
  }

}
/**
  * an InPhase for Runnables
  *
  * By enclosing the activity in the doRun method of this class the profiler (if enabled) is informed
  * and the statistics can be gathered
  */

object RunnableInPhase {
  def apply(global: Global, phase:Phase, comment:String)(fn: => Unit)(implicit executionContext: ExecutionContext) = {
    new RunnableInPhase(global, phase, comment)(fn)
  }
}
class RunnableInPhase(global: Global, phase:Phase, comment:String)(fn: => Unit) extends InPhase(global, phase, comment) with Runnable {
  final def run(): Unit = doAction(fn)
}

/**
  * an InPhase for Futures
  *
  * By enclosing the activity in this wrapper the profiler (if enabled) is informed
  * and the statistics can be gathered
  */
object FutureInPhase {
  def apply[T](global: Global, phase:Phase, comment:String)(fn: => T)(implicit executionContext: ExecutionContext) = {
    val inPhase = new FutureInPhase(global, phase, comment)(fn)
    Future(inPhase.exec())
  }
}

class FutureInPhase[T](global: Global, phase:Phase, comment:String)(fn: => T) extends InPhase(global, phase, comment) {
  final def exec() = doAction(fn)
}

sealed trait AsyncHelper {
  //TODO replace this
  def wrap(executor: ThreadPoolExecutor) = executor

  //TODO replace this
  def baseGroup = Thread.currentThread.getThreadGroup

  def newFixedThreadPool
    (nThreads: Int,
     info:String, priority : Int = Thread.NORM_PRIORITY) = 0

}

object AsyncHelper {
  def apply(global: Global, phase: Phase): AsyncHelper = {
    if (global.settings.YprofileEnabled) new ProfilingAsyncHelper(global, phase)
    else new BasicAsyncHelper(global, phase)
  }

  private abstract class BaseAsyncHelper(global: Global, phase: Phase) extends AsyncHelper {
    val baseGroup = new ThreadGroup(s"scalac-${phase.name}")
    private def childGroup(name: String) = new ThreadGroup(baseGroup, name)
    type ProfileGroup <: AnyRef

    protected def wrapRunnable(r:Runnable, pGroup:ProfileGroup) : Runnable
    protected class CommonThreadFactory(shortId: String, pGroup:ProfileGroup,
                                      daemon: Boolean = true,
                                      priority: Int) extends ThreadFactory {
      private val group: ThreadGroup = childGroup(shortId)
      private val threadNumber: AtomicInteger = new AtomicInteger(1)
      private val namePrefix = s"${baseGroup.getName}-$shortId-"

      override def newThread(r: Runnable): Thread = {
        val wrapped = wrapRunnable(r, pGroup)
        val t: Thread = new Thread(group, wrapped, namePrefix + threadNumber.getAndIncrement, 0)
        if (t.isDaemon != daemon) t.setDaemon(daemon)
        if (t.getPriority != priority) t.setPriority(priority)
        t
      }
    }
  }
  private class BasicAsyncHelper(global: Global, phase: Phase) extends BaseAsyncHelper(global, phase){
    override type ProfileGroup = Void

    override protected def wrapRunnable(r: Runnable, pGroup: Void): Runnable = r
  }


  private class ProfilingAsyncHelper(global: Global, phase: Phase) extends BaseAsyncHelper(global, phase){
    override type ProfileGroup = ProfileData

    override protected def wrapRunnable(r: Runnable, pGroup: ProfileData): Runnable =
      new Runnable {
        override def run(): Unit =
          try r.run finally processThreadStats()
      }

    class ProfileData {
      import scala.collection.JavaConverters._
      val running = Collections.newSetFromMap(new ConcurrentHashMap[ThreadProfileData,java.lang.Boolean]).asScala

      object runningTotals  {
        var threadCount = 0

        var idleNs = 0L
        var runningNs = 0L

        var lastStartNs = 0L
        var lastEndNs = 0L
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


    private class SinglePhaseInstrumentedThreadPoolExecutor
    ( corePoolSize: Int, maximumPoolSize: Int, keepAliveTime: Long, unit: TimeUnit,
      workQueue: BlockingQueue[Runnable], threadFactory: ThreadFactory, handler: RejectedExecutionHandler
    ) extends ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory,handler) {
      val firstStart = new AtomicLong
      val lastEnd = new AtomicLong
      val allThreadTime = new AtomicLong
      val allTaskCount = new AtomicLong

      object localData extends ThreadLocal[ThreadProfileData] {
        override def initialValue() = new ThreadProfileData
      }

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

      override def terminated(): Unit = {
        publishStats()
        super.terminated()
      }
    }
    def publishStats(): Unit = {

    }
    def processThreadStats(): Unit ={

    }
  }
}