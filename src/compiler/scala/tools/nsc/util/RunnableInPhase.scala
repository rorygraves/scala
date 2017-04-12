package scala.tools.nsc.util

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Awaitable, Future}
import scala.tools.nsc.{Global, Phase}
object RunnableInPhase {
  val idGen = new AtomicInteger
}
/**
  * a wrapper to allow Runnables to be associated to a phase. Primarily useful for profiling
  *
  * When you are running a compilation task that involved some activty on a background thread
  * (not the one running [[Global.compileUnits]]) the profiler is not aware of that thread and so cannot account
  * for the activity.
  *
  * By enclosing the activity in the doRun method of this class the profiler (if enabled) is informed
  * and the statistics can be gathered
  */
abstract class RunnableInPhase(global: Global, val phase:Phase, val comment:String) extends Runnable {

  final val id = RunnableInPhase.idGen.incrementAndGet()
  private val profiler = global.currentRun.profiler
  private var idle = 0L
  profiler.registerRunnable(this)


  final def run(): Unit = {
    profiler.beforeRunnable(this)
    try doRun()
    finally profiler.afterRunnable(this, idle)
  }

  /**
    * the compilation activity
    */
  def doRun() : Unit

  /**
    * if the compilation activity has some idle time waiting on a future, thn this can be recorded by
    * using this method to do the wait for you. This allow the profiler to distinguish idle time (waiting for some
    * related activity to complete, from for example waiting or IO
    * @param future the future that you are waiting on
    * @param duration the maximum duration to wait
    */
  def idle(future: Future[_], duration:Duration = Duration.Inf): Unit = {
    if (!future.isCompleted) {
      val start = System.nanoTime()
      try Await.ready(future, duration)
      finally idle += (System.nanoTime() - start)
    }
  }

}