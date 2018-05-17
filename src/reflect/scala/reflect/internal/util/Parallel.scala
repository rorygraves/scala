package scala.reflect.internal.util

import java.lang
import java.util.concurrent.atomic.AtomicInteger

import scala.reflect.internal.Reporter

object Parallel {

  class WorkerThread(group: ThreadGroup, target: Runnable, name: String,
                     stackSize: Long) extends Thread(group, target, name, stackSize)


  final class BufferedReporter extends Reporter {

    override type Severity = Int
    override val INFO = 0
    override val WARNING = 1
    override val ERROR = 2

    private val counts = Array(0, 0, 0)

    override def count(severity: Int): Int = counts(severity)

    override def resetCount(severity: Int): Unit = {
      counts(severity) = 0
    }

    private[this] var buffered = List.empty[BufferedMessage]

    override protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
      assertOnWorker
      buffered = BufferedMessage(pos, msg, severity, force) :: buffered
      count(severity) += 1
    }

    def flushTo(reporter: Reporter) = {
      assertOnMain
      val sev = Array(reporter.INFO, reporter.WARNING, reporter.ERROR)
      // TODO int the long term we may want th order the messages, to ensure determinsism
      // but for the moment this should be good enough
      buffered.reverse.foreach {
        msg =>
          reporter.info0(msg.pos, msg.msg, sev(msg.severity), msg.force)
      }
      buffered = Nil
    }

    private case class BufferedMessage(pos: Position, msg: String, severity: Severity, force: Boolean)

  }

  def WorkerThreadLocal[T <: AnyRef](valueOnWorker: => T, valueOnMain: => T) = new WorkerOrMainThreadLocal[T](valueOnWorker, valueOnMain)
  def WorkerThreadLocal[T <: AnyRef](valueOnWorker: => T) = new WorkerThreadLocal[T](valueOnWorker)


  // `WorkerThreadLocal` allows us to detect some value to be read/write on the main thread,
  // and we want to discover violations of that rule.

  class WorkerThreadLocal[T](valueOnWorker: => T) {

    private val worker: ThreadLocal[T] = new ThreadLocal[T] {
      override def initialValue(): T = valueOnWorker
    }

    def get: T = if (isWorkerThread) worker.get() else main

    def set(value: T): Unit = if (isWorkerThread) worker.set(value) else main = value

    def reset(): Unit = {
      assertOnMain
      worker.remove()
    }
    protected def main: T  = throw new IllegalStateException ("not allowed on main thread")
    protected def main_=(v: T)  = throw new IllegalStateException ("not allowed on main thread")
  }
  // `WorkerOrMainThreadLocal` allows us to have different (sub)type of values on main and worker threads.
  // It's useful in cases like reporter, when on workers we want to just store messages and on main we want to print them,

  class WorkerOrMainThreadLocal[T](valueOnWorker: => T, valueOnMain: => T) extends WorkerThreadLocal(valueOnWorker){

    protected override var main = valueOnMain
    override def reset(): Unit = {
      super.reset()
      main = valueOnMain
    }
  }

  class Counter() {
    private val count = new AtomicInteger

    def reset() = {
      assertOnMain
      count.set(0)
    }

    def incrementAndGet = {
      count.incrementAndGet
    }
    override def toString: String = s"Counter[$count]"
  }

  def assertOnMain = {
    assert(!isWorkerThread)
  }

  def assertOnWorker = {
    assert(isWorkerThread)
  }

  def isWorkerThread = {
    Thread.currentThread.isInstanceOf[WorkerThread] || mainIsWorker.get()
  }

  //this needs to be a ThreadLocal to support parallel compilation
  val mainIsWorker = new ThreadLocal[java.lang.Boolean] {
    override def initialValue(): lang.Boolean = false
  }

  @inline def asWorkerThread[T](fn: => T): T = {
    assertOnMain
    mainIsWorker.set(true)
    try fn finally mainIsWorker.set(false)
  }

}
