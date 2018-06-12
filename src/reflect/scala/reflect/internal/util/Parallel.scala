package scala.reflect.internal.util

import java.util.concurrent.atomic.AtomicInteger

object Parallel {

  class Counter(initial: Int = 0) {
    private val count = new AtomicInteger
    count.set(initial)

    @inline final def get: Int = count.get()

    @inline final def reset(): Unit = {
      assertOnMain()
      count.set(0)
    }

    @inline final def incrementAndGet(): Int = count.incrementAndGet

    @inline final def getAndIncrement(): Int = count.getAndIncrement

    @inline final def set(v: Int): Int = count.getAndSet(v)

    @inline final override def toString: String = s"Counter[$count]"
  }

  object Counter {
    def apply(initial: Int = 0): Counter = new Counter(initial)
  }

  // Wrapper for `synchronized` method. In future could provide additional logging, safety checks, etc.
  def synchronizeAccess[T <: Object, U](obj: T)(block: => U): U = {
    obj.synchronized[U](block)
  }

  def WorkerThreadLocal[T <: AnyRef](valueOnWorker: => T, valueOnMain: => T) = new WorkerOrMainThreadLocal[T](valueOnWorker, valueOnMain)

  def WorkerThreadLocal[T <: AnyRef](valueOnWorker: => T) = new WorkerThreadLocal[T](valueOnWorker)

  abstract class AbstractThreadLocal[T](valueOnWorker: => T, valueOnMain: => T) {
    private var main: T = null.asInstanceOf[T]

    private val worker: ThreadLocal[T] = new ThreadLocal[T] {
      override def initialValue(): T = valueOnWorker
    }

    @inline final def get: T = {
      if (isWorker.get()) worker.get()
      else {
        if (main == null) main = valueOnMain
        main
      }
    }

    @inline final def set(value: T): Unit = if (isWorker.get()) worker.set(value) else main = value

    @inline final def reset(): Unit = {
      worker.remove()
      main = valueOnMain
    }
  }

  // `WorkerThreadLocal` detects reads/writes of given value on the main thread and
  // and report such violations by throwing exception.
  class WorkerThreadLocal[T](valueOnWorker: => T) extends AbstractThreadLocal(valueOnWorker, throw new IllegalStateException("not allowed on main thread"))

  // `WorkerOrMainThreadLocal` allows us to have different type of values on main and worker threads.
  // It's useful in cases like reporter, when on workers we want to just store messages and on main we want to print them,
  class WorkerOrMainThreadLocal[T](valueOnWorker: => T, valueOnMain: => T) extends AbstractThreadLocal(valueOnWorker, valueOnMain)

  // Asserts that current execution happens on the main thread
  @inline final def assertOnMain(): Unit = {
    if (ParallelSettings.areAssertionsEnabled) assert(!isWorker.get())
  }

  // Asserts that current execution happens on the worker thread
  @inline final def assertOnWorker(): Unit = {
    if (ParallelSettings.areAssertionsEnabled) assert(isWorker.get())
  }

  // Runs block of the code in the 'worker thread' mode
  // All unit processing should always happen in the worker thread
  @inline final def asWorkerThread[T](fn: => T): T = {
    val previous = isWorker.get()
    isWorker.set(true)
    try fn finally isWorker.set(previous)
  }

  // Runs block of the code in the 'main thread' mode.
  // In 'main' mode we mostly sets/resets global variables, initialize contexts,
  // and orchestrate processing of phases/units
  @inline final def asMainThread[T](fn: => T): T = {
    val previous = isWorker.get()
    isWorker.set(false)
    try fn finally isWorker.set(previous)
  }

  // ThreadLocal variable which allows us to mark current thread as main or worker.
  // This is important because real main thread is not necessarily always running 'main' code.
  // Good example may be tests which all runs in one main thread, although often processes units
  // (what conceptually should always happen in workers).
  // Because there is much more entry points to unit processing than to Global,
  // it's much easier to start with assuming everything is initially worker thread
  // and just mark main accordingly when needed.
  private val isWorker: ThreadLocal[Boolean] = new ThreadLocal[Boolean] {
    override def initialValue(): Boolean = true
  }
}