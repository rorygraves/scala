package scala.reflect.runtime

object ThreadIdentityAwareThreadLocal {
  def apply[T](valueOnWorker: => T) = new ThreadIdentityAwareThreadLocal[T](valueOnWorker, null.asInstanceOf[T])
  def apply[T](valueOnWorker: => T,  valueOnMain: => T) = new ThreadIdentityAwareThreadLocal[T](valueOnWorker, valueOnMain)
}

class ThreadIdentityAwareThreadLocal[T](valueOnWorker: => T, valueOnMain: => T) {
  var main: T = valueOnMain

  private val worker: ThreadLocal[T] = new ThreadLocal[T] {
    override def initialValue(): T = valueOnWorker
  }

  private def isOnMainThread = {
    val isMain = "main".equals(Thread.currentThread().getName)
    assert(!(isMain && valueOnMain == null), "Variable cannot be accessed on the main thread")
    isMain
  }

  def get: T = if (isOnMainThread) main else worker.get()

  def set(value: T): Unit = if (isOnMainThread) main = value else worker.set(value)

  def reset(): Unit = {
    main = valueOnMain
    worker.set(valueOnWorker)
  }
}
