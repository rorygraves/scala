package scala.reflect.internal.util

import java.util.concurrent.atomic.AtomicInteger

object Parallel {

  var isParallel = false

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
  @inline def synchronizeAccess[T <: Object, U](obj: T)(block: => U): U = {
    if (isParallel) obj.synchronized[U](block) else block
  }

  class Lock {
    @inline final def apply[T](op: => T) = synchronizeAccess(this)(op)
  }

  class AbstractThreadLocal[T](initial: T, shouldFailOnMain: Boolean) {
    private var main: T = initial

     private[this] lazy val worker: ThreadLocal[T] = new ThreadLocal[T] {
       override def initialValue(): T = initial
     }

    @inline final def get: T = {
      if (isParallel && isWorker.get()) worker.get()
      else {
        if (isParallel && shouldFailOnMain) throw new IllegalStateException("not allowed on main thread")
        main
      }
    }

    @inline final def set(value: T): Unit =
      if (isParallel && isWorker.get()) worker.set(value) else main = value

    @inline final def reset(): Unit = {
      worker.remove()
      main = initial
    }
  }

  class LazyThreadLocal[T](initial: => T, shouldFailOnMain: Boolean = false) {
    private var main: T = null.asInstanceOf[T]

    private[this] lazy val worker: ThreadLocal[T] = new ThreadLocal[T] {
      override def initialValue(): T = initial
    }

    @inline final def get: T = {
      if (isParallel && isWorker.get()) worker.get()
      else {
        if (isParallel && shouldFailOnMain) throw new IllegalStateException("not allowed on main thread")
        if (main == null) main = initial
        main
      }
    }

    @inline final def set(value: T): Unit =
      if (isParallel && isWorker.get()) worker.set(value) else main = value

    @inline final def reset(): Unit = {
      worker.remove()
      main = initial
    }
  }

  type WorkerThreadLocal[T] = AbstractThreadLocal[T]
  // `WorkerThreadLocal` detects reads/writes of given value on the main thread and
  // and report such violations by throwing exception.
  def WorkerThreadLocal[T](valueOnWorker: T) = new AbstractThreadLocal(valueOnWorker, true)
  // `WorkerOrMainThreadLocal` allows us to have different type of values on main and worker threads.
  // It's useful in cases like reporter, when on workers we want to just store messages and on main we want to print them,

  type WorkerOrMainThreadLocal[T] = AbstractThreadLocal[T]
  def WorkerOrMainThreadLocal[T](valueOnWorker: T) = new AbstractThreadLocal(valueOnWorker, false)

  // Asserts that current execution happens on the main thread
  @inline final def assertOnMain(): Unit = {
    if (ParallelSettings.areAssertionsEnabled && isParallel) assert(!isWorker.get())
  }

  // Asserts that current execution happens on the worker thread
  @inline final def assertOnWorker(): Unit = {
    if (ParallelSettings.areAssertionsEnabled && isParallel) assert(isWorker.get())
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
  val isWorker: ThreadLocal[Boolean] = new ThreadLocal[Boolean] {
    override def initialValue(): Boolean = true
  }
}