package scala.reflect.internal.util

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.{immutable, mutable}

object Parallel {

  var isParallel: Boolean = false

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
    @inline final def apply(initial: Int = 0): Counter = new Counter(initial)
  }

  val locksCount = mutable.Map[Object, Int]().withDefault(_ => 0)
  val locksPerClass = mutable.Map[Class[_], Int]().withDefault(_ => 0)
  val waitingTime = mutable.Map[Class[_], Long]().withDefault(_ => 0)
  val lockTime = mutable.Map[Class[_], Long]().withDefault(_ =>  0)
  object StatLock


  private[this] lazy val currentLocks: ThreadLocal[mutable.ListBuffer[Any]] = new ThreadLocal[mutable.ListBuffer[Any]] {
    override def initialValue(): mutable.ListBuffer[Any] = mutable.ListBuffer[Any]()
  }
  private[this] lazy val repeated: ThreadLocal[Int] = new ThreadLocal[Int] {
    override def initialValue(): Int = 0
  }
  val uniqueLockSets = mutable.Set[immutable.List[Any]]()

  object Repeated
  object Sym


  // Wrapper for `synchronized` method. In future could provide additional logging, safety checks, etc.
  @inline final def synchronizeAccess[T <: Object, U](obj: T)(block: => U): U = {
    var startTime, endTime, lockedTime: Long = 0
    if (isParallel) {
      startTime = System.currentTimeMillis
      val ret = obj.synchronized[U] {
        lockedTime = System.currentTimeMillis

        val tmpObj = if (obj.getClass.getName.startsWith("scala.reflect.internal.Symbols$")) Sym else obj

        if (currentLocks.get().nonEmpty && (currentLocks.get().last.getClass == tmpObj.getClass  || currentLocks.get().last.getClass == Repeated.getClass)) {
          if (currentLocks.get().last.getClass == Repeated.getClass) repeated.set(repeated.get() + 1)
          else currentLocks.get += Repeated
        } else currentLocks.get += tmpObj


        StatLock.synchronized { uniqueLockSets.add(currentLocks.get.toList) }
        val r = block

        if (currentLocks.get().last.getClass == Repeated.getClass) {
          if (repeated == 0) currentLocks.get.dropRight(1)
          else repeated.set(repeated.get() - 1)
        } else currentLocks.get.dropRight(1)


        endTime = System.currentTimeMillis
        r
      }
      StatLock.synchronized {
        val clazz = obj.getClass
        locksCount(obj) += 1
        locksPerClass(clazz) += 1
        waitingTime(clazz) += (lockedTime - startTime)
        lockTime(clazz) += (endTime - lockedTime)
      }
      ret
    } else block
  }

  class Lock {
    @inline final def apply[T](op: => T): T = synchronizeAccess(this)(op)
  }

  @inline final def EagerWorkerThreadLocal[T](initial: T, shouldFailOnMain: Boolean = true) =
    new EagerWorkerThreadLocal(initial, shouldFailOnMain)

  class EagerWorkerThreadLocal[T](@specialized initial: T, shouldFailOnMain: Boolean) {
    private var main: T = null.asInstanceOf[T]

    private[this] lazy val worker: ThreadLocal[T] = new ThreadLocal[T] {
      override def initialValue(): T = initial
    }

    @inline final def get: T = {
      if (isParallel && isWorker.get()) worker.get()
      else {
        //if (isParallel && shouldFailOnMain) throw new IllegalStateException("not allowed on main thread")
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

  class WorkerThreadLocal[T](initial: => T, shouldFailOnMain: Boolean = false) {
    private var main: T = null.asInstanceOf[T]

    private[this] lazy val worker: ThreadLocal[T] = new ThreadLocal[T] {
      override def initialValue(): T = initial
    }

    @inline final def get: T = {
      if (isParallel && isWorker.get()) worker.get()
      else {
        //if (isParallel && shouldFailOnMain) throw new IllegalStateException("not allowed on main thread")
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

  // `WorkerThreadLocal` detects reads/writes of given value on the main thread and
  // and report such violations by throwing exception.
  @inline final def WorkerThreadLocal[T](valueOnWorker: => T) = new WorkerThreadLocal(valueOnWorker, true)
  // `WorkerOrMainThreadLocal` allows us to have different type of values on main and worker threads.
  // It's useful in cases like reporter, when on workers we want to just store messages and on main we want to print them,

  type WorkerOrMainThreadLocal[T] = WorkerThreadLocal[T]
  @inline final def WorkerOrMainThreadLocal[T](valueOnWorker: => T) = new WorkerThreadLocal(valueOnWorker, false)

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