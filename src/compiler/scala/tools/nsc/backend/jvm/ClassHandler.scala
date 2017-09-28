package scala.tools.nsc.backend.jvm

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.tools.nsc.Settings

private[jvm] sealed trait ClassHandler {
  val lock: AnyRef

  def startUnit() = ()
  def endUnit() = ()

  def startProcess(clazz: GeneratedClass): Unit

  def globalOptimise()

  def pending(): List[(GeneratedClass, Future[Unit])]

  def initialise() = ()

  val postProcessor:PostProcessor
}
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
//
//    new HackedClassHandler(postProcessor, writer, cfWriter)
  }


//  class HackedClassHandler(val postProcessor: PostProcessor, w:WritingClassHandler, cfWriter:ClassfileWriter, val lock:AnyRef) extends WritingClassHandler {
//    private val bufferBuilder = List.newBuilder[GeneratedClass]
//    override def initialise() = bufferBuilder.clear()
//
//    override def pending(): List[(GeneratedClass, Future[Unit])] = {
//      bufferBuilder.result() foreach (postProcessor.sendToDisk(_, cfWriter))
//      bufferBuilder.result() map {
//          (_,Future.successful(()))
//      }
//    }
//
//    override def startProcess(clazz: GeneratedClass): Unit = {
//      bufferBuilder += clazz
//    }
//  }

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
    private implicit val ec = ExecutionContext.fromExecutor(java.util.concurrent.Executors.newFixedThreadPool(maxThreads))

    override def startProcess(clazz: GeneratedClass): Unit = {
      inUnit += clazz
    }

    def pending(): List[(GeneratedClass, Future[Unit])] = {
      val result = bufferBuilder.result()
      bufferBuilder.clear()
      result
    }
    override def initialise(): Unit = {
      super.initialise()
      bufferBuilder.clear()
    }
    override def toString: String = s"AsyncWriting[threads:$maxThreads writer:$cfWriter]"

    val inUnit = ListBuffer.empty[GeneratedClass]

    override def startUnit(): Unit = super.startUnit()

    override def endUnit(): Unit = {
      inUnit foreach { clazz =>
        val future = Future(postProcessor.sendToDisk(clazz, cfWriter))
        bufferBuilder += ((clazz, future))
      }
      inUnit.clear()
      //temp hack to ensure single threaded access to data
//      bufferBuilder.result().foreach {
//        case (cls, f) => Await.result(f, Duration.Inf)
//      }
    }
  }



}
