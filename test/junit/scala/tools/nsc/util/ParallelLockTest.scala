package scala.tools.nsc.util

import scala.collection.mutable.ArrayBuffer
import scala.tools.nsc.{Global, Settings}
import org.junit._
import org.junit.Assert._

class ParallelLockTest {

  case class LockLog(entry: Boolean, thread: Int, cycle: Int)
  val settings = new Settings
  val st = new Global(settings, null)
  val lockManager = st.lockManager
  val messages = new ArrayBuffer[LockLog]

  @Test def setSimpleRoot(): Unit = {
    val lock = lockManager.rootLock("1", false)
    val threads = for (threadId <- 1 to 100) yield {
      new Thread(s"$threadId") {
        for (cycle <- 1 to 100) {
          lock.withLock {
            messages += LockLog(true, threadId, cycle)
            Thread.sleep(0, 1)
            messages += LockLog(false, threadId, cycle)
          }
        }
      }
    }
    threads.foreach(_.start)
    threads foreach (_.join(1000))
    checkLog()
  }
  @Test def setUnowneredRoot(): Unit = {
    val threadCount = 100
    val lock = lockManager.rootLock("1", false)
    val children = (0 to threadCount + 1) map { i => lockManager.childLock(lock, i.toString, false) }
    val threads = for (threadId <- 1 to threadCount) yield {
      new Thread(s"$threadId") {
        children(threadId).withLock {
          for (cycle <- 1 to 100) {
            lock.withUnorderedLock {
              children(threadId + 1).withLock {
                children(threadId - 1).withLock {

                  messages += LockLog(true, threadId, cycle)
                  Thread.sleep(0, 1)
                  messages += LockLog(false, threadId, cycle)
                }
              }
            }
          }
        }
      }
    }
    threads.foreach(_.start)
    threads foreach (_.join(1000))
    checkLog()
  }
  def checkLog() {
    //check that the locks are valid
    val res = messages.toVector
    assertEquals(20000, res.size)
    for (next <- Range(0, res.size,2)) {
      val LockLog(lock, thread, cycle) = res(next)
      assertTrue(lock)
      val LockLog(lock2, thread2, cycle2) = res(next + 1)
      assertFalse(lock2)
      assertSame(thread, thread2)
      assertSame(cycle, cycle2)
    }
  }
}
