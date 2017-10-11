package scala.tools.nsc.backend.jvm

import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.reflect.internal.util.{NoPosition, SourceFile}
import scala.tools.nsc.Settings
import scala.tools.nsc.io.AbstractFile
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
//    case e: FileConflictException =>
//      backendReporting.error(NoPosition, s"error writing $className: ${e.getMessage}")
//    case e: java.nio.file.FileSystemException =>
//      if (compilerSettings.debug)
//        e.printStackTrace()
//      backendReporting.error(NoPosition, s"error writing $className: ${e.getClass.getName} ${e.getMessage}")


private[jvm] object ClassHandler {

  def apply(cfWriter: ClassfileWriter, settings:Settings, postProcessor: PostProcessor) = {
    val lock = postProcessor.bTypes.frontendAccess.frontendLock
    val unitInfoLookup = settings.outputDirs.getSingleOutput match {
      case Some(dir) => new SingleUnitInfo(dir)
      case None => new LookupUnitInfo(postProcessor.bTypes.frontendAccess)
    }

    val writer = settings.YmaxAddWriterThreads.value match {
      case 0 => new SyncWritingClassHandler(unitInfoLookup, postProcessor, cfWriter, lock)
      case x => new AsyncWritingClassHandler(unitInfoLookup, postProcessor, cfWriter, lock, x)
    }

    val res = if (settings.optInlinerEnabled || settings.optClosureInvocations)
    new GlobalOptimisingGeneratedClassHandler(postProcessor, writer, lock)
    else writer

    println(s"writer $writer")
    println(s"cfWriter $cfWriter")
    println(s"res $res")

    println(s"optAddToBytecodeRepository ${settings.optAddToBytecodeRepository}")
    println(s"optBuildCallGraph ${settings.optBuildCallGraph}")
    println(s"optInlinerEnabled ${settings.optInlinerEnabled}")
    println(s"optClosureInvocations ${settings.optClosureInvocations}")

    println(s"YmaxWriterThreads ${settings.YmaxAddWriterThreads.value}")

    res
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

    override def toString: String = s"GloballyOmpimising[$underlying]"
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

    /**
      * provides a ability to create any parent directories that may be needed, before we write the files
      * if writing is done asynchnonously this enables the IO operation to occur in parallel off the main thread
      * @param unitProcess
      */
    protected def ensureDirectories(unitProcess: UnitResult): Unit
    protected def postProcessUnit(unitProcess:UnitResult)

    override def close(): Unit = cfWriter.close()

  }
  private final class SyncWritingClassHandler(val unitInfoLookup: UnitInfoLookup, val postProcessor: PostProcessor, val cfWriter: ClassfileWriter, val lock:AnyRef) extends WritingClassHandler {
    private val bufferBuilder = List.newBuilder[UnitResult]
    override def initialise(): Unit = {
      super.initialise()
      bufferBuilder.clear()
    }

    //we do everything synchronously
    override protected def ensureDirectories(unitProcess: UnitResult): Unit = ()

    override protected def postProcessUnit(unitProcess: UnitResult): Unit = {
      unitProcess.takeClasses foreach {
        postProcessor.sendToDisk(unitProcess, _, cfWriter)
      }
      unitProcess.completedUnit()

      Await.result(unitProcess.result.future, Duration.Inf)
    }

    override def complete(): Unit = ()

    override def toString: String = s"SyncWriting[$cfWriter]"
  }

  private final class AsyncWritingClassHandler(val unitInfoLookup: UnitInfoLookup, val postProcessor: PostProcessor, val cfWriter: ClassfileWriter, val lock:AnyRef, maxThreads:Int) extends WritingClassHandler {
    // max pending units. If more than maxQueue are pending we will run in current thread
    // so this provides some back pressure, so we can allow the files to be written and recover memory
    // and encorages prallelism in the more expensive post processing
    // The setting is based of expecting that processing one unit in the foreground will not take longer then
    // existing back end writers and th time to generate the next unit
    //  this may require further tuning ....
    val maxQueue = maxThreads * 2 + 2
    val javaExecutor = new ThreadPoolExecutor(maxThreads, maxThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueue))
    javaExecutor.setRejectedExecutionHandler(new CallerRunsPolicy)
    private implicit val ec = ExecutionContext.fromExecutor(java.util.concurrent.Executors.newFixedThreadPool(maxThreads))

    override def toString: String = s"AsyncWriting[additional threads:$maxThreads writer:$cfWriter]"

    override protected def ensureDirectories(unitProcess: UnitResult): Unit = {
      cfWriter.ensureDirectories(ec, unitProcess)
    }

    override protected def postProcessUnit(unitProcess: UnitResult): Unit =
      unitProcess.task = Future {
        unitProcess.takeClasses foreach {postProcessor.sendToDisk(unitProcess, _, cfWriter)}
        unitProcess.completedUnit()
      }

    override def complete(): Unit = {
      // This way it is easier to test, as the results are deterministic
      // the the loss of potential performance is probably minimal
      pending().foreach {
        unitResult: UnitResult =>
          try {
            Await.result(unitResult.task, Duration.Inf)
            Await.result(unitResult.result.future, Duration.Inf)
          } catch {
            case NonFatal(t) =>
              t.printStackTrace
              postProcessor.bTypes.frontendAccess.backendReporting.error(NoPosition, s"unable to write ${unitResult.source} $t")
          }
      }

    }
  }

}
//we avoid the lock on frontendSync for the common case, when compiling to a single target
sealed trait UnitInfoLookup {
  def outputDir(source:AbstractFile) : AbstractFile
}
final class SingleUnitInfo(constantOutputDir:AbstractFile) extends UnitInfoLookup {
  override def outputDir(source: AbstractFile) = constantOutputDir
}
final class LookupUnitInfo(frontendAccess: PostProcessorFrontendAccess) extends UnitInfoLookup {
  override def outputDir(source: AbstractFile) = frontendAccess.compilerSettings.outputDirectoryFor(source)
}
sealed trait SourceUnit extends CompletionHandler[Integer, AsynchronousFileChannel]{
  val outputDir: AbstractFile
  val outputPath: java.nio.file.Path
  def addOperation(): Unit

}
final class UnitResult(unitInfoLookup: UnitInfoLookup, classes_ : List[GeneratedClass],  val source:SourceFile) extends SourceUnit {
  lazy val outputDir = unitInfoLookup.outputDir(source.file)

  lazy val outputPath = outputDir.file.toPath
  private var classes : List[GeneratedClass] = classes_

  def copyClasses = classes

  def takeClasses(): List[GeneratedClass] = {
    val c = classes
    classes = Nil
    c
  }
  /** the main async task submitted onto the scheduler */
  var task: Future[Unit] = _

  /** the final completion which may occur after the task completes
    * this allows the use of async completions*/
  val result = Promise[Unit]

  private val pendingOperations = new AtomicInteger(1)
  override def failed(exc: Throwable, channel: AsynchronousFileChannel) = result.tryFailure(exc)
  override def completed(x: Integer, channel: AsynchronousFileChannel) = {
    try channel.close()
    catch {
      case NonFatal(t) =>
        t.printStackTrace()
        result.tryFailure(t)
      case t:Throwable =>
        t.printStackTrace()
        result.tryFailure(t); throw t
    } finally {
      if (pendingOperations.decrementAndGet() == 0) result.trySuccess(())
    }
  }

  def addOperation(): Unit = pendingOperations.incrementAndGet()

  def completedUnit(): Unit = {
    if (pendingOperations.decrementAndGet() == 0) result.trySuccess(())
  }

}
