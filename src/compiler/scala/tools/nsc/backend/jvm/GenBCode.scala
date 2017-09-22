/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package jvm

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.internal.util.Statistics
import scala.tools.asm.Opcodes

abstract class GenBCode extends SubComponent {
  self =>
  import global._

  val postProcessorFrontendAccess: PostProcessorFrontendAccess = new PostProcessorFrontendAccess.PostProcessorFrontendAccessImpl(global)

  val bTypes: BTypesFromSymbols[global.type] = new { val frontendAccess = postProcessorFrontendAccess } with BTypesFromSymbols[global.type](global)

  val codeGen: CodeGen[global.type] = new { val bTypes: self.bTypes.type = self.bTypes } with CodeGen[global.type](global)

  val postProcessor: PostProcessor { val bTypes: self.bTypes.type } = new { val bTypes: self.bTypes.type = self.bTypes } with PostProcessor

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)

  class HackedClassHandler extends WritingClassHandler(null) {
    private val bufferBuilder = List.newBuilder[GeneratedClass]
    override def initialise() = bufferBuilder.clear()

    override def pending(): List[(GeneratedClass, Future[Unit])] = bufferBuilder.result() map {(_,null)}

    override def startProcess(clazz: GeneratedClass): Unit = {
      bufferBuilder += clazz
    }
  }
  private[GenBCode] sealed abstract class GeneratedClassHandler extends GeneratedClassProcessor{
    def globalOptimise()

    def writer: WritingClassHandler

    def initialise() = ()
  }

  private class GlobalOptimisingGeneratedClassHandler(val writer: WritingClassHandler) extends GeneratedClassHandler {
    private val bufferBuilder = List.newBuilder[GeneratedClass]

    override def startProcess(clazz: GeneratedClass): Unit = bufferBuilder += clazz

    override def globalOptimise(): Unit = {
      val allClasses = bufferBuilder.result()
      postProcessor.runGlobalOptimizations(allClasses)
      allClasses foreach writer.startProcess
    }

    override def initialise(): Unit = {
      bufferBuilder.clear()
      writer.initialise()
    }
  }

  sealed abstract class WritingClassHandler(val cfWriter: ClassfileWriter) extends GeneratedClassHandler{
    def pending(): List[(GeneratedClass, Future[Unit])]
    final override def globalOptimise(): Unit = ()
    final def writer = this
  }
  private class SyncWritingClassHandler(cfWriter: ClassfileWriter) extends WritingClassHandler(cfWriter) {
    private val bufferBuilder = List.newBuilder[GeneratedClass]
    override def initialise(): Unit = {
      super.initialise()
      bufferBuilder.clear()
    }
    override def startProcess(clazz: GeneratedClass): Unit = {
      bufferBuilder += clazz
    }
    def pending(): List[(GeneratedClass, Future[Unit])] = {
      bufferBuilder.result() map { clazz:GeneratedClass =>
        val promise = Promise.fromTry(scala.util.Try(postProcessor.sendToDisk(clazz, cfWriter)))
        (clazz, promise.future)
      }
    }
  }

  private class AsyncWritingClassHandler(cfWriter: ClassfileWriter, maxThreads:Int) extends WritingClassHandler(cfWriter) {
    private val bufferBuilder = List.newBuilder[(GeneratedClass, Future[Unit])]
    private implicit val ec = ExecutionContext.fromExecutor(java.util.concurrent.Executors.newFixedThreadPool(maxThreads))

    override def startProcess(clazz: GeneratedClass): Unit = {
      val future = Future(postProcessor.sendToDisk(clazz, cfWriter))
      bufferBuilder += ((clazz, future))
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

  }

  class BCodePhase(prev: Phase) extends StdPhase(prev) {
    override def description = "Generate bytecode from ASTs using the ASM library"

    override val erasedTypes = true

    private val globalOptsEnabled = {
      import postProcessorFrontendAccess._
      compilerSettings.optInlinerEnabled || compilerSettings.optClosureInvocations
    }
    private val generatedHandler:GeneratedClassHandler = {
      val cfWriter = postProcessor.classfileWriter.get
      val writer = settings.YmaxWriterThreads.value match {
        case 0 => new SyncWritingClassHandler(cfWriter)
        case x => new AsyncWritingClassHandler(cfWriter, x)
      }

      if (globalOptsEnabled) new GlobalOptimisingGeneratedClassHandler(writer)
      else writer

      new HackedClassHandler()
    }
    def apply(unit: CompilationUnit): Unit = {
      codeGen.genUnit(unit, generatedHandler)
    }

    override def run(): Unit = {
      BackendStats.timed(BackendStats.bcodeTimer) {
        try {
          initialize()
          val writer = postProcessor.classfileWriter.get
          BackendStats.timed(BackendStats.bcodeGenStat) {
            super.run() // invokes `apply` for each compilation unit
          }
          generatedHandler.globalOptimise()
          generatedHandler.writer.pending().foreach {
            case ((clazz, result)) =>
              postProcessor.sendToDisk(clazz, writer)
          }
        } finally {
          // When writing to a jar, we need to close the jarWriter. Since we invoke the postProcessor
          // multiple times if (!globalOptsEnabled), we have to do it here at the end.
          postProcessor.classfileWriter.get.close()
        }
      }
    }

    /**
     * Several backend components have state that needs to be initialized in each run, because
     * it depends on frontend data that may change between runs: Symbols, Types, Settings.
     */
    private def initialize(): Unit = {
      val initStart = Statistics.startTimer(BackendStats.bcodeInitTimer)
      scalaPrimitives.init()
      bTypes.initialize()
      codeGen.initialize()
      postProcessorFrontendAccess.initialize()
      postProcessor.initialize()
      generatedHandler.initialise()
      Statistics.stopTimer(BackendStats.bcodeInitTimer, initStart)
    }
  }
}

object GenBCode {
  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  final val PublicStatic = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC
  final val PublicStaticFinal = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL

  val CLASS_CONSTRUCTOR_NAME = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"
}
sealed abstract class GeneratedClassProcessor {
  def startProcess(clazz: GeneratedClass): Unit
}

