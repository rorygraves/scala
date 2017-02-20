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
  */
abstract class RunnableInPhase(global: Global, val phase:Phase, val comment:String) extends Runnable {

  val id = RunnableInPhase.idGen.incrementAndGet()
  private val profiler = global.currentRun.profiler
  private var idle = 0L
  profiler.registerRunnable(this)


  final def run(): Unit = {
    profiler.beforeRunnable(this)
    try doRun()
    finally profiler.afterRunnable(this, idle)
  }
  def doRun() : Unit

  def idle(future: Future[_], duration:Duration = Duration.Inf): Unit = {
    if (!future.isCompleted) {
      val start = System.nanoTime()
      try Await.ready(future, duration)
      finally idle += (System.nanoTime() - start)
    }
  }

}