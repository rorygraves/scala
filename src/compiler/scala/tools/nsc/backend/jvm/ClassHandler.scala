package scala.tools.nsc.backend.jvm

import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Settings
import scala.tools.nsc.io.AbstractFile

private[jvm] sealed trait ClassHandler {
  val lock: AnyRef

  def startUnit(unit:SourceFile) = ()
  def endUnit() = ()

  def startProcess(clazz: GeneratedClass): Unit

  def globalOptimise()

  def pending(): List[(GeneratedClass, Future[Unit])]

  def initialise() = ()

  val postProcessor:PostProcessor
}
//sealed
//trait UnderlyingWriter {
//
//}
//final class FileWriter extends UnderlyingWriter {
//
//}
//final class JarWriter extends UnderlyingWriter {
//
//}
private[jvm] object ClassHandler {

  def apply(settings:Settings, postProcessor: PostProcessor, lock:AnyRef) = {
    val cfWriter = postProcessor.classfileWriter.get
    val writer = settings.YmaxWriterThreads.value match {
      case 0 => new SyncWritingClassHandler(postProcessor, cfWriter, lock)
      case x => new AsyncWritingClassHandler(postProcessor, cfWriter, lock, x)
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

    println(s"YmaxWriterThreads ${settings.YmaxWriterThreads.value}")

    res
  }

  private class GlobalOptimisingGeneratedClassHandler(val postProcessor: PostProcessor, val underlying: WritingClassHandler, val lock:AnyRef) extends ClassHandler {
    private val bufferBuilder = List.newBuilder[GeneratedClass]

    override def startProcess(clazz: GeneratedClass): Unit = bufferBuilder += clazz

    override def globalOptimise(): Unit = {
      val allClasses = bufferBuilder.result()
      postProcessor.runGlobalOptimizations(allClasses)
      allClasses foreach underlying.startProcess
    }

    override def pending() = underlying.pending()

    override def initialise(): Unit = {
      bufferBuilder.clear()
      underlying.initialise()
    }

    override def toString: String = s"GloballyOmpimising[$underlying]"
  }

  sealed trait WritingClassHandler extends ClassHandler{
    final def globalOptimise(): Unit = ()
  }
  private final class SyncWritingClassHandler(val postProcessor: PostProcessor, cfWriter: ClassfileWriter, val lock:AnyRef) extends WritingClassHandler {
    private val bufferBuilder = List.newBuilder[GeneratedClass]
    override def initialise(): Unit = {
      super.initialise()
      bufferBuilder.clear()
    }
    override def startProcess(clazz: GeneratedClass): Unit = {
      bufferBuilder += clazz
    }
    def pending(): List[(GeneratedClass, Future[Unit])] = {
      val result = bufferBuilder.result() map { clazz:GeneratedClass =>
        val promise = Promise.fromTry(scala.util.Try(postProcessor.sendToDisk(clazz, cfWriter)))
        (clazz, promise.future)
      }
      bufferBuilder.clear()
      result
    }
    override def toString: String = s"SyncWriting[$cfWriter]"
  }

  private final class AsyncWritingClassHandler(val postProcessor: PostProcessor, cfWriter: ClassfileWriter, val lock:AnyRef, maxThreads:Int) extends WritingClassHandler {
    private val bufferBuilder = List.newBuilder[(GeneratedClass, Future[Unit])]
    //max pending units. If moe than maxQueue are pending we will run in current thread
    //so this provides some back pressure, so we can allow the files to be written and recover memory
    val maxQueue = 50
    val javaExecutor = new ThreadPoolExecutor(maxThreads, maxThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueue))
    javaExecutor.setRejectedExecutionHandler(new CallerRunsPolicy)
    private implicit val ec = ExecutionContext.fromExecutor(java.util.concurrent.Executors.newFixedThreadPool(maxThreads))

    override def startProcess(clazz: GeneratedClass): Unit = {
      inUnit += clazz
    }

    def pending(): List[(GeneratedClass, Future[Unit])] = {
      val result = bufferBuilder.result()
      bufferBuilder.clear()
      javaExecutor.shutdownNow()
      result
    }
    override def initialise(): Unit = {
      super.initialise()
      bufferBuilder.clear()
    }
    override def toString: String = s"AsyncWriting[threads:$maxThreads writer:$cfWriter]"

    val inUnit = ListBuffer.empty[GeneratedClass]

    override def endUnit(): Unit = {
      //TODO sumbit a unit not a class
      inUnit foreach { clazz =>
        val future = Future(postProcessor.sendToDisk(clazz, cfWriter))
        bufferBuilder += ((clazz, future))
      }
      inUnit.clear()
    }
  }



}
