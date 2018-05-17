package scala.tools.nsc.util

object ThreadIdentityAwareThreadLocal {
  def apply[T](valueOnWorker: => T) = new ThreadIdentityAwareThreadLocal[T](valueOnWorker, null.asInstanceOf[T])
  def apply[T](valueOnWorker: => T,  valueOnMain: => T) = new ThreadIdentityAwareThreadLocal[T](valueOnWorker, valueOnMain)
}

// `ThreadIdentityAwareThreadLocal` allows us to have different (sub)type of values on main and worker threads.
// It's useful in cases like reporter, when on workers we want to just store messages and on main we want to print them,
// but also in the cases when we do not expect some value to be read/write on the main thread,
// and we want to discover violations of that rule.
class ThreadIdentityAwareThreadLocal[T](valueOnWorker: => T, valueOnMain: => T) {
  var main: T = valueOnMain

  private val worker: ThreadLocal[T] = new ThreadLocal[T] {
    override def initialValue(): T = valueOnWorker
  }

  // That logic may look a little bit funky because we need to consider cases
  // where there is only one thread which is both main and worker at a given time.
  private def isOnMainThread: Boolean = {
    val currentThreadName = Thread.currentThread().getName
    def isMainDefined = currentThreadName.startsWith("main") && valueOnMain != null
    def isWorkerDefined = currentThreadName.contains("-worker")

    assert(isMainDefined || isWorkerDefined, "Variable cannot be accessed on the main thread")

    isMainDefined
  }

  def get: T = if (isOnMainThread) main else worker.get()

  def set(value: T): Unit = if (isOnMainThread) main = value else worker.set(value)

  def reset(): Unit = {
    main = valueOnMain
    worker.set(valueOnWorker)
  }
}
