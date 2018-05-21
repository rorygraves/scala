package scala.reflect.internal.util

import java.util.concurrent.atomic.AtomicInteger

object Parallel {

  class WorkerThread(group: ThreadGroup, target: Runnable, name: String,
                     stackSize: Long) extends Thread(group, target, name, stackSize)

  def WorkerThreadLocal[T <: AnyRef](valueOnWorker: => T, valueOnMain: => T) = new WorkerOrMainThreadLocal[T](valueOnWorker, valueOnMain)

  def WorkerThreadLocal[T <: AnyRef](valueOnWorker: => T) = new WorkerThreadLocal[T](valueOnWorker)

  // `WorkerOrMainThreadLocal` allows us to have different (sub)type of values on main and worker threads.
  // It's useful in cases like reporter, when on workers we want to just store messages and on main we want to print them,
  class WorkerOrMainThreadLocal[T](valueOnWorker: => T, valueOnMain: => T) {

    private var main: T = null.asInstanceOf[T]

    private val worker: ThreadLocal[T] = new ThreadLocal[T] {
      override def initialValue(): T = valueOnWorker
    }

    final def get: T = {
      if (isWorkerThread) worker.get()
      else {
        if (main == null) main = valueOnMain
        main
      }
    }

    final def set(value: T): Unit = if (isWorkerThread) worker.set(value) else main = value

    final def reset(): Unit = {
      worker.remove()
      main = valueOnMain
    }
  }

  // `WorkerThreadLocal` allows us to detect some value to be read/write on the main thread,
  // and we want to discover violations of that rule.
  class WorkerThreadLocal[T](valueOnWorker: => T)
    extends WorkerOrMainThreadLocal(valueOnWorker, throw new IllegalStateException("not allowed on main thread"))

  class Counter {
    private val count = new AtomicInteger

    def get: Int = count.get()

    def reset(): Unit = {
      assertOnMain()
      count.set(0)
    }

    def incrementAndGet(): Int = count.incrementAndGet

    def getAndIncrement(): Int = count.getAndIncrement

    override def toString: String = s"Counter[$count]"
  }

  def assertOnMain(): Unit = {
    assert(!isWorkerThread)
  }

  def assertOnWorker(): Unit = {
    assert(isWorkerThread)
  }

  def isWorkerThread: Boolean = {
    Thread.currentThread.isInstanceOf[WorkerThread] || isWorker.get()
  }

  // This needs to be a ThreadLocal to support parallel compilation
  val isWorker: ThreadLocal[Boolean] = new ThreadLocal[Boolean] {
    override def initialValue(): Boolean = false
  }

  @inline def asWorkerThread[T](fn: => T): T = {
    assertOnMain()
    isWorker.set(true)
    try fn finally isWorker.set(false)
  }

}