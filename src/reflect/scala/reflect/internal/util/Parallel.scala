package scala.reflect.internal.util

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.{HashSet => JHashSet}
import java.util.{IdentityHashMap => IHashMap}
import java.lang.{Long => JLong}
import java.util.concurrent.locks.ReentrantLock

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

  private object lockManager extends ThreadLocal[LockManager] {
    //assumes that we dont run parallel except with global
    override def initialValue(): LockManager = new LockManager
  }

  def profileLocks(doProfiling: Boolean): Unit = {
    assertOnMain()
    if (doProfiling) lockManager.set(new ProfileLockManager)
    else lockManager.set(new LockManager)
  }

  def getLockManager = lockManager.get

  def installLockManager(lockMgr: LockManager): Unit = {
    assertOnWorker()
    lockManager.set(lockMgr)
  }
  def lockedWriteAccess[U](locked: AnyRef)(block: => U): U = {
    lockManager.get().lockedWriteAccess(locked)(block)
  }
  def withWriteLock[U](lock: LockWithStats)(block: => U): U = {
    lockManager.get().withWriteLock(lock)(block)
  }
  class LockManager {
    val autoLocks = java.util.Collections.synchronizedMap(new IHashMap[AnyRef, LockWithStats])
    def lockedWriteAccess[U](locked: AnyRef)(block: => U): U = {
      val lock = autoLocks.computeIfAbsent(locked, l => new LockWithStats(l.toString))
      withWriteLock(lock)(block)
    }
    def withWriteLock[U](lock: LockWithStats)(block: => U): U = {
      lock.writeLock.lock
      try block finally lock.writeLock.unlock
    }
    import collection.JavaConverters._
    def lockStats = autoLocks.values().asScala.toList.map {_.snapAndReset}
  }
  class ProfileLockManager extends LockManager{
    override def withWriteLock[U](lock: LockWithStats)(block: => U): U = {
      if (lock.writeLock.tryLock())
        lock.uncontendedWrites.incrementAndGet()
      else {
        lock.contendedWrites.incrementAndGet()
        val start = System.nanoTime()
        lock.writeLock.lock
        lock.contendedWriteNs.addAndGet(System.nanoTime() - start)
      }
      try block finally lock.writeLock.unlock
    }
  }
  private val lockIdGen = new AtomicLong

  case class LockStats(name: String, id: Long, contendedWrites: Int, uncontendedWrites: Int, contendedWriteNs: Long) {
    def accessCount = contendedWrites + uncontendedWrites
  }
  class LockWithStats(name: String) {
    val writeLock = new ReentrantLock
    val id: Long =  lockIdGen.incrementAndGet()

//    val contendedReads = new AtomicInteger
//    val uncontendedReads = new AtomicInteger

    val contendedWrites = new AtomicInteger
    val uncontendedWrites = new AtomicInteger

    val contendedWriteNs = new AtomicLong
//    val contendedReadNs = new AtomicLong

    def snapAndReset = {
      val snapped = new LockStats(name, id, contendedWrites.get(), uncontendedWrites.get, contendedWriteNs.get)
      contendedWrites.addAndGet(-snapped.contendedWrites)
      uncontendedWrites.addAndGet(-snapped.uncontendedWrites)
      contendedWriteNs.addAndGet(-snapped.contendedWriteNs)
      snapped
    }
  }

  val locks: ThreadLocal[JHashSet[JLong]] = new ThreadLocal[JHashSet[JLong]]() {
    override def initialValue(): JHashSet[JLong] = new JHashSet[JLong]()
  }

  // Wrapper for `synchronized` method. In future could provide additional logging, safety checks, etc.
  @inline final def synchronizeAccess[T <: Object, U](obj: T)(block: => U): U = {
    if (isParallel) obj.synchronized[U](block) else block
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

  class WorkerThreadLocal[T](initial: => T, shouldFailOnMain: Boolean = false) {
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