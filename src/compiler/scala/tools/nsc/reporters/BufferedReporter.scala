package scala.tools.nsc.reporters

import scala.reflect.internal.util.Parallel.{assertOnMain, assertOnWorker}
import scala.reflect.internal.util.Position

/* Simple Reporter which allows us to accumulate messages over time
 * and then at suitable time forward them to other reporter using `flushTo` method
 */
final class BufferedReporter extends Reporter {
  private[this] var buffered = List.empty[BufferedMessage]

  protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    assertOnWorker()
    buffered = BufferedMessage(pos, msg, severity, force) :: buffered
    severity.count += 1
  }

  def flushTo(reporter: Reporter): Unit = {
    assertOnMain()
    val sev = Array(reporter.INFO, reporter.WARNING, reporter.ERROR)
    buffered.reverse.foreach {
      msg =>
        reporter.info(msg.pos, msg.msg, sev(msg.severity.id), msg.force)
    }
    buffered = Nil
  }

  private case class BufferedMessage(pos: Position, msg: String, severity: Severity, force: Boolean)
}