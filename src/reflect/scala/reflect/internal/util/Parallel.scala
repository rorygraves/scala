package scala.reflect.internal.util

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.{HashSet => JHashSet}
import java.util.{IdentityHashMap => IHashMap}
import java.lang.{Long => JLong}
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

  case class LockStats(name: String, id: Long,
                       acquireNoWaitCount: Int,
                       acquireWaitCount: Int,
                       acquireWaitNs: Long,
                       acquireUnorderedNoWaitCount: Int,
                       acquireUnorderedWaitCount: Int,
                       acquireUnorderedWaitNs: Long) {

    def accessCount = acquireNoWaitCount + acquireWaitCount + acquireUnorderedNoWaitCount + acquireUnorderedWaitNs
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

  // Wrapper for `synchronized` method. In future could provide additional logging, safety checks, etc.
  @inline final def synchronizeAccess[T <: Object, U](obj: T)(block: => U): U = {
    if (isParallel) obj.synchronized[U](block) else block
  }


  abstract class LockedBy(lock: Lock) {
    @inline final def withLock[T](fn: => T): T = lock.withLock(fn)
  }

  /** A lock where there is no symbol table in scope, and no possibility of reentry
    *
    * @param name the name of the lock
    */
  def simpleLock(name: String): SimpleLock = new SimpleLock(name)

  sealed class SimpleLock private[Parallel](name: String) {
    protected val underlying = new ReentrantLock()

    override def toString: String = s"SimpleLock[${name}"

    /**
      * acquire the lock
      * if you own the lock, or the parent it cheap and cant deadlock
      *
      * otherwise it is illegal to call [[lock]] if
      * you hold locks with the same parent (i.e. siblings)
      * you hold locks for which this is the ancestor (i.e. transitive parent)
      * you hold locks for which this is a descendant (i.e. transitive child)
      */
    def lock(): Unit = underlying.lock

    /**
      * unlock the lock
      */
    def unlock(): Unit = underlying.unlock

    @inline final def withLock[T](fn: => T): T = {
      lock()
      try fn finally unlock()
    }
  }

  sealed abstract class Lock(name: String) extends SimpleLock(name) {
    protected def fullName: String

    protected val level: Int

    override def toString: String = s"Lock[${fullName}"


    /**
      * acquire the lock, where you may hold some direct children already
      * if you own the lock, or the parent it cheap and cant deadlock
      *
      * if you hold direct child locks then these locks ( and their children recursively) may be unlocked prior to
      * acquiring this lock and relocaked afterwards
      *
      * otherwise it is illegal to call [[unorderedLock]] if
      * you hold locks with the same parent (i.e. siblings)
      * you hold locks for which this is the ancestor (i.e. transitive parent)
      * you hold locks for which this is a descendant (i.e. transitive child)
      */
    def unorderedLock() = underlying.lock

    @inline final def withUnorderedLock[T](fn: => T): T = {
      unorderedLock()
      try fn finally unlock()
    }

    def tryLock(): Boolean

    def isHeldByCurrentThread: Boolean
  }

  sealed abstract class LockManager[LT <: AbstractLock] {
    def lockStats(): Seq[LockStats] = Nil

    def childLock(parent: LT, name: String, perRun: Boolean): LT

    def rootLock(name: String, perRun: Boolean): LT

    type LockType = LT
  }

  sealed abstract class AbstractLock(name: String) extends Lock(name)
  object NoLockManager extends LockManager[NoLock.type ] {
    override def childLock(parent: NoLock.type, name: String, perRun: Boolean): NoLock.type = NoLock
    override def rootLock(name: String, perRun: Boolean): NoLock.type = NoLock
  }

  object NoLock extends AbstractLock("single") {
    override protected def fullName: String = "single"

    override protected val level: Int = 0

    override def tryLock(): Boolean = true

    override def isHeldByCurrentThread: Boolean = true
  }

  final class RealLock (private[Parallel] val manager: RealLockManager,
                                                        private[Parallel] val parent: RealLock,
                                                        name: String,
                                                        perRun: Boolean) extends AbstractLock (name) {
    def fullName: String = s"${if (parent eq null) "" else parent.fullName}/$name${if (perRun) "- perRun" else ""}"

    val level = if (parent eq null) 0 else parent.level + 1

    override def tryLock(): Boolean = true

    override def isHeldByCurrentThread: Boolean = true

    val id = lockIdGen.incrementAndGet

    def addStatsTo(builder: mutable.Growable[LockStats]): Unit = {
      builder += snapAndReset()
      children foreach {
        _.addStatsTo(builder)
      }
    }

    private def snapAndReset() = {
      val snapped = new LockStats(name, id,
        acquireNoWaitCount.get(), acquireWaitCount.get, acquireWaitNs.get,
        acquireUnorderedNoWaitCount.get(), acquireUnorderedWaitCount.get, acquireUnorderedWaitNs.get)
      acquireNoWaitCount.addAndGet(-snapped.acquireNoWaitCount)
      acquireWaitCount.addAndGet(-snapped.acquireWaitCount)
      acquireWaitNs.addAndGet(-snapped.acquireWaitNs)
      acquireUnorderedNoWaitCount.addAndGet(-snapped.acquireUnorderedNoWaitCount)
      acquireUnorderedWaitCount.addAndGet(-snapped.acquireUnorderedWaitCount)
      acquireUnorderedWaitNs.addAndGet(-snapped.acquireUnorderedWaitNs)
      snapped
    }


    private val acquireNoWaitCount = new AtomicInteger()
    private val acquireWaitCount = new AtomicInteger()
    private val acquireWaitNs = new AtomicLong()

    private val acquireUnorderedNoWaitCount = new AtomicInteger()
    private val acquireUnorderedWaitCount = new AtomicInteger()
    private val acquireUnorderedWaitNs = new AtomicLong()
    if (parent ne null) parent.addChild(this)


    @tailrec def checkParentNotOwned(parent: RealLock): Unit = {
      if (parent ne null) {
        assert(!parent.underlying.isHeldByCurrentThread, s"cant acquire $this when we own $parent")
        checkParentNotOwned(parent.parent)
      }
    }

    private def checkNoChildLocks(current: RealLock): Unit = {
      for (child <- current.children) {
        assert(!child.underlying.isHeldByCurrentThread)
        checkNoChildLocks(child)
      }

    }

    private def checkNoSiblings(): Unit = {
      if (parent ne null) {
        for (sibling <- parent.children) {
          assert((sibling eq this) || !sibling.underlying.isHeldByCurrentThread)
        }
      }
    }

    private def checkLockInvariants(): Unit = {
      if ((parent ne null) && !parent.underlying.isHeldByCurrentThread) {
        checkParentNotOwned(parent.parent)
        checkNoSiblings
      }
      checkNoChildLocks(this)
    }

    private def checkUnorderedLockInvariants(): Unit = {
      //we cant own the parent
      //if we did we would have already skipped a level
      //if we expect to have children
      checkParentNotOwned(parent)
      //we can hold direct children only
      for (child <- children) {
        if (!child.underlying.isHeldByCurrentThread)
          checkNoChildLocks(child)

      }
    }

    private val children = new WeakHashSet[RealLock]

    private[RealLock] def addChild(child: RealLock): Unit = children.add(child)

    /**
      * all locks that are held by children or their descendents
      *
      * @param locks
      * @return
      */
    private def childLocksHeld(locks: ArrayBuffer[RealLock] = new ArrayBuffer[RealLock]()): ArrayBuffer[RealLock] = {
      locks += this
      for (child <- children if (child.underlying.isHeldByCurrentThread))
        child.childLocksHeld(locks)
      locks
    }

    override def lock(): Unit = {
      if (underlying.isHeldByCurrentThread) {
        underlying.lock()
      } else {
        var checked = false
        if (manager.checkAllInvariants) {
          checkLockInvariants()
          checked = true
        }
        if (underlying.tryLock()) {
          acquireNoWaitCount.incrementAndGet()
        } else {
          val start = System.nanoTime()
          acquireWaitCount.incrementAndGet()
          while (!underlying.tryLock(10, TimeUnit.SECONDS)) {
            manager.trace("waiting for lock")
            if (!checked)
              checkLockInvariants()
            checked = true
          }
          acquireWaitNs.addAndGet(System.nanoTime() - start)
        }
      }
    }

    override def unorderedLock(): Unit = {
      if (underlying.isHeldByCurrentThread) {
        underlying.lock()
      } else {
        var checked = false
        if (manager.checkAllInvariants) {
          checkUnorderedLockInvariants()
          checked = true
        }
        if (underlying.tryLock()) {
          acquireUnorderedNoWaitCount.incrementAndGet()
        } else {
          val start = System.nanoTime()
          val locks = childLocksHeld() map {
            lock =>

              val underlying = lock.underlying
              val held = underlying.getHoldCount
              nTimes(held, underlying.unlock())
              (lock, held)
          }

          acquireUnorderedWaitCount.incrementAndGet()
          while (!underlying.tryLock(10, TimeUnit.SECONDS)) {
            manager.trace("waiting for Unordered lock")
            if (!checked)
              checkUnorderedLockInvariants()
            checked = true
          }
          locks foreach {
            case (lock, count) =>
              val underlying = lock.underlying
              nTimes(count, underlying.lock())
          }
          acquireUnorderedWaitNs.addAndGet(System.nanoTime() - start)
        }
      }
    }

    override def unlock(): Unit = super.unlock()

  }

  private val lockIdGen = new AtomicInteger()

//  class BaseLockManager extends LockManager[BaseLock[BaseLock]] {
//    private val single = new NoopLock(this)
//
//    def childLock(parent: NoopLock, name: String, perRun: Boolean) = single
//
//    def rootLock(name: String, perRun: Boolean) = single
//
//  }
//
//  private[Parallel] class NoopLock(owner:NoopLockManager) extends BaseLock[NoopLock](owner, null, "single", false)
//
//  //  01246580037

  final class RealLockManager extends LockManager[RealLock] {
    private val lockIdGen = new AtomicLong

    def childLock(parent: LockType, name: String, perRun: Boolean) = new RealLock(this, parent, name, perRun)

    private val allRoots = new ConcurrentHashMap[LockType, LockType]()

    override def rootLock(name: String, perRun: Boolean) = {
      val res = new RealLock(this, null, name, perRun)
      allRoots.put(res, res)
      res
    }

    val checkAllInvariants = true

    override def lockStats(): Seq[LockStats] = {
      import scala.collection.JavaConverters._

      val builder = Seq.newBuilder[LockStats]
      allRoots.keys().asScala foreach {
        _.addStatsTo(builder)
      }

      builder.result()
    }

    def trace(s: String) = println(s)
  }


  @tailrec private def nTimes(n: Int, fn: => Unit): Unit = {
    if (n > 0) {
      fn
      nTimes(n - 1, fn)
    }
  }


}