package scala.tools.nsc.backend.jvm

import java.io.IOException
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.reflect.internal.util.{NoPosition, Position, SourceFile}
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.backend.jvm.PostProcessorFrontendAccess.BackendReporting
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.profile.AsyncHelper
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

private[jvm] sealed trait ClassHandler {
  def close(): Unit

  def complete(): Unit

  val lock: AnyRef

  def endUnit(unit:SourceFile)

  def startProcess(clazz: GeneratedClass): Unit

  def initialise() = ()

  val postProcessor:PostProcessor
}

private[jvm] object ClassHandler {

  def apply(asyncHelper: AsyncHelper, cfWriter: ClassfileWriter, settings:Settings, postProcessor: PostProcessor) = {
    val lock = postProcessor.bTypes.frontendAccess.frontendLock
    val unitInfoLookup = settings.outputDirs.getSingleOutput match {
      case Some(dir) => new SingleUnitInfo(postProcessor.bTypes.frontendAccess, dir)
      case None => new LookupUnitInfo(postProcessor.bTypes.frontendAccess)
    }

    val osExec = settings.YosIoThreads.value match {
      case 0 => null
      case maxThreads => asyncHelper.newUnboundedQueueFixedThreadPool(maxThreads, "osExec", Thread.NORM_PRIORITY + 2)
    }
    cfWriter.exec = osExec

    val realCfWriter = settings.YIoWriterThreads.value match {
      case 0 => cfWriter
      case maxThreads =>
        val javaExecutor = asyncHelper.newBoundedQueueFixedThreadPool(maxThreads, maxThreads * 5, new CallerRunsPolicy, "cfWriter", Thread.NORM_PRIORITY + 1)
        new AsyncClassfileWriter(javaExecutor, maxThreads, cfWriter)
    }
    val writer = settings.YaddBackendThreads.value match {
      case 0 => new SyncWritingClassHandler(true, unitInfoLookup, postProcessor, realCfWriter, lock)
      case -1 => new SyncWritingClassHandler(false, unitInfoLookup, postProcessor, realCfWriter, lock)
      case maxThreads =>
        // the queue size is taken to be large enough to ensure that the a 'CallerRun' will not take longer to
        // run that it takes to exhaust the queue for the backend workers
        val queueSize = if (settings.YmaxQueue.isSetByUser) settings.YmaxQueue.value else maxThreads * 2 + 2
        // when the queue is full, the main thread will no some background work
        // so this provides back-pressure
        val queue = new ArrayBlockingQueue[Runnable](queueSize)
        // we assign a higher priority to the background workers as they are removing load from the system
        // and they cannot block the main thread indefinitely
        val javaExecutor = asyncHelper.newBoundedQueueFixedThreadPool(maxThreads, queueSize, new CallerRunsPolicy, "non-ast")
        new AsyncWritingClassHandler(unitInfoLookup, postProcessor, realCfWriter, lock, maxThreads, javaExecutor, javaExecutor.getQueue)
    }

    if (settings.optInlinerEnabled || settings.optClosureInvocations)
      new GlobalOptimisingGeneratedClassHandler(postProcessor, writer, lock)
    else writer
  }

  private class GlobalOptimisingGeneratedClassHandler(val postProcessor: PostProcessor, val underlying: WritingClassHandler, val lock:AnyRef) extends ClassHandler {
    private val bufferBuilder = List.newBuilder[GeneratedClass]

    override def close(): Unit = underlying.close()

    override def startProcess(clazz: GeneratedClass): Unit = bufferBuilder += clazz

    override def complete(): Unit = {
      globalOptimise()
      underlying.complete()
    }

    override def endUnit(unit: SourceFile): Unit = ()

    private def globalOptimise(): Unit = {
      val allClasses = bufferBuilder.result()
      postProcessor.runGlobalOptimizations(allClasses)

      //replay the units to underlying
      var current = allClasses.head
      underlying.startProcess(current)
      for (next <- allClasses.tail) {
        if (next.sourceFile ne current.sourceFile) {
          underlying.endUnit(current.sourceFile)
        }
        underlying.startProcess(next)
        current = next
      }
      underlying.endUnit(current.sourceFile)
    }

    override def initialise(): Unit = {
      bufferBuilder.clear()
      underlying.initialise()
    }

    override def toString: String = s"GloballyOptimising[$underlying]"
  }

  sealed trait WritingClassHandler extends ClassHandler{
    val unitInfoLookup: UnitInfoLookup
    val cfWriter: ClassfileWriter

    protected val pendingBuilder = List.newBuilder[UnitResult]
    private val inUnit = ListBuffer.empty[GeneratedClass]

    override def startProcess(clazz: GeneratedClass): Unit = {
      inUnit += clazz
    }

    protected def pending(): List[UnitResult] = {
      val result = pendingBuilder.result()
      pendingBuilder.clear()
      result
    }
    override def initialise(): Unit = {
      super.initialise()
      pendingBuilder.clear()
    }

    override def endUnit(unit:SourceFile): Unit = {
      val unitProcess = new UnitResult(unitInfoLookup, inUnit.result, unit)
      inUnit.clear()
      ensureDirectories(unitProcess)
      postProcessUnit(unitProcess)

      pendingBuilder += unitProcess
    }

    protected val javaExecutor :Executor
    protected implicit val executionContext = ExecutionContext.fromExecutor(javaExecutor)

    /**
      * provides a ability to create any parent directories that may be needed, before we write the files
      * if writing is done asynchronously this enables the IO operation to occur in parallel off the main thread
      * @param unitProcess the unit being processed
      */
    protected def ensureDirectories(unitProcess: UnitResult): Unit
    protected def postProcessUnit(unitProcess: UnitResult): Unit = {
      unitProcess.task = Future {
        // we 'take' classes to reduce the memory pressure
        // as soon as the class is consumed and written , we release its pointers
        unitProcess.takeClasses foreach {
          postProcessor.sendToDisk(unitProcess, _, cfWriter)
        }
        unitProcess.completedUnit()
      }
    }
    def depth = ""

    override def close(): Unit = cfWriter.close()

    override def complete(): Unit = {
      val directBackendReporting = postProcessor.bTypes.frontendAccess.directBackendReporting
      def stealWhileWaiting(unitResult: UnitResult, fut: Future[Unit]): Unit = {
        while (!fut.isCompleted)
          tryStealing match {
            case Some(r:Runnable) => r.run()
            case None => Await.ready(fut, Duration.Inf)
        }
        //we know that they are complete by we need to check for exception
        //but first get any reports
        unitResult.relayReports(directBackendReporting)
        fut.value.get.get
      }

      // This way it is easier to test, as the results are deterministic
      // the the loss of potential performance is probably minimal
      pending().foreach {
        unitResult: UnitResult =>
          try {
            stealWhileWaiting(unitResult, unitResult.task)
            stealWhileWaiting(unitResult, unitResult.result.future)
          } catch {
            case NonFatal(t) =>
              t.printStackTrace()
              postProcessor.bTypes.frontendAccess.backendReporting.error(NoPosition, s"unable to write ${unitResult.source} $t")
          }
      }
    }
    def tryStealing:Option[Runnable]
  }
  private final class SyncWritingClassHandler(val syncUnit:Boolean, val unitInfoLookup: UnitInfoLookup, val postProcessor: PostProcessor, val cfWriter: ClassfileWriter, val lock:AnyRef) extends WritingClassHandler {

    object javaExecutor extends Executor {
      def execute(r: Runnable): Unit = r.run()
    }
    //we do everything synchronously
    override protected def ensureDirectories(unitProcess: UnitResult): Unit = ()

    override protected def postProcessUnit(unitProcess: UnitResult): Unit = {
      super.postProcessUnit(unitProcess)
      if (syncUnit) Await.ready(unitProcess.result.future, Duration.Inf)
    }

    override def toString: String = s"SyncWriting wait:$syncUnit [$cfWriter]"

    override def tryStealing: Option[Runnable] = None
  }

  private final class AsyncWritingClassHandler(val unitInfoLookup: UnitInfoLookup, val postProcessor: PostProcessor, val cfWriter: ClassfileWriter, val lock:AnyRef,
                                               maxThreads:Int, override val javaExecutor : ExecutorService, queue:BlockingQueue[Runnable]) extends WritingClassHandler {
    val otherJavaExec = Executors.newFixedThreadPool(2,new CommonThreadFactory("scalac-async-nonast", priority = Thread.NORM_PRIORITY -1))
    val otherExec =  ExecutionContext.fromExecutor(otherJavaExec)
    cfWriter.exec = otherJavaExec

    override def depth = ""+queue.size()

    override def toString: String = s"AsyncWriting[additional threads:$maxThreads writer:$cfWriter]"

    override protected def ensureDirectories(unitProcess: UnitResult): Unit = {
      cfWriter.ensureDirectories(otherExec, unitProcess)
    }

    override def close(): Unit = {
      super.close()
      javaExecutor.shutdownNow()
      otherJavaExec.shutdownNow()
    }

    override def tryStealing: Option[Runnable] = Option(queue.poll())
  }

}
//we avoid the lock on frontendSync for the common case, when compiling to a single target
sealed trait UnitInfoLookup {
  def outputDir(source:AbstractFile) : AbstractFile
  val frontendAccess: PostProcessorFrontendAccess
}
final class SingleUnitInfo(val frontendAccess: PostProcessorFrontendAccess, constantOutputDir:AbstractFile) extends UnitInfoLookup {
  override def outputDir(source: AbstractFile) = constantOutputDir
}
final class LookupUnitInfo(val frontendAccess: PostProcessorFrontendAccess) extends UnitInfoLookup {
  lazy val outputDirectories = frontendAccess.compilerSettings.outputDirectories
  override def outputDir(source: AbstractFile) = outputDirectories.outputDirFor(source)
}
sealed trait SourceUnit extends CompletionHandler[Integer, (AsynchronousFileChannel, String)]{
  def withBufferedReporter[T](fn: => T): T

  val outputDir: AbstractFile
  val outputPath: java.nio.file.Path
  def addOperation(): Unit
  def endOperation(result: Try[Unit]): Unit
  def sourceFile:AbstractFile

}
final class UnitResult(unitInfoLookup: UnitInfoLookup, classes_ : List[GeneratedClass], val source:SourceFile) extends SourceUnit with BackendReporting {
  lazy val outputDir = unitInfoLookup.outputDir(source.file)

  override def sourceFile = source.file

  lazy val outputPath = outputDir.file.toPath
  private var classes: List[GeneratedClass] = classes_

  def copyClasses = classes

  def takeClasses(): List[GeneratedClass] = {
    val c = classes
    classes = Nil
    c
  }

  /** the main async task submitted onto the scheduler */
  var task: Future[Unit] = _

  /** the final completion which may occur after the task completes
    * this allows the use of async completions */
  val result = Promise[Unit]()

  private val pendingOperations = new AtomicInteger(1)

  override def failed(exc: Throwable, channelInfo: (AsynchronousFileChannel, String)) = result.tryFailure(exc)

  override def completed(x: Integer, channelInfo: (AsynchronousFileChannel, String)) = {
    try channelInfo._1.close()
    catch {
      case e: IOException =>
        if (unitInfoLookup.frontendAccess.compilerSettings.debug)
          e.printStackTrace()
        unitInfoLookup.frontendAccess.backendReporting.error(NoPosition, s"error closing file for ${channelInfo._2}: ${e.getMessage}")
    } finally {
      if (pendingOperations.decrementAndGet() == 0) result.trySuccess(())
    }
  }

  def addOperation(): Unit = pendingOperations.incrementAndGet()

  def completedUnit(): Unit = endOperation(Success())

  def endOperation(opResult: Try[Unit]): Unit = opResult match {
    case _: Success[_] =>
      if (pendingOperations.decrementAndGet() == 0) result.trySuccess(())
    case Failure(f) =>
      result.tryFailure(f)
      pendingOperations.decrementAndGet()
  }

  def relayReports(backendReporting: BackendReporting) = this.synchronized {
    if (bufferedReports nonEmpty) {
      for (report: Report <- bufferedReports.reverse) {
        report.relay(backendReporting)
      }
    }
  }

  private var bufferedReports = List.empty[Report]

  override def withBufferedReporter[T](fn: => T) = unitInfoLookup.frontendAccess.withLocalReporter(this)(fn)

  override def inlinerWarning(pos: Position, message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportInlinerWarning(pos, message))

  override def error(pos: Position, message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportError(pos, message))

  override def inform(message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportInform(message))

  override def log(message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportLog(message))

  private sealed trait Report {
    def relay(backendReporting: BackendReporting): Unit
  }

  private class ReportInlinerWarning(pos: Position, message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.inlinerWarning(pos, message)
  }

  private class ReportError(pos: Position, message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.error(pos, message)
  }

  private class ReportInform(message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.inform(message)
  }

  private class ReportLog(message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.log(message)
  }

}

