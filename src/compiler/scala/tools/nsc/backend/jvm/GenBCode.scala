/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package jvm

import scala.tools.asm.Opcodes
import scala.tools.nsc.profile.AsyncHandler

abstract class GenBCode extends SubComponent {
  self =>
  import global._
  import statistics._

  val postProcessorFrontendAccess: PostProcessorFrontendAccess = new PostProcessorFrontendAccess.PostProcessorFrontendAccessImpl(global)

  val bTypes: BTypesFromSymbols[global.type] = new { val frontendAccess = postProcessorFrontendAccess } with BTypesFromSymbols[global.type](global)

  val codeGen: CodeGen[global.type] = new { val bTypes: self.bTypes.type = self.bTypes } with CodeGen[global.type](global)

  val postProcessor: PostProcessor { val bTypes: self.bTypes.type } = new {
    val bTypes: self.bTypes.type = self.bTypes
  } with PostProcessor(statistics)

  val phaseName = "jvm"

  override def newPhase(prev: Phase) = new BCodePhase(prev)


  class BCodePhase(prev: Phase) extends StdPhase(prev) {
    override def description = "Generate bytecode from ASTs using the ASM library"

    override val erasedTypes = true

    private var generatedHandler:ClassHandler = _
    def apply(unit: CompilationUnit): Unit = {
      codeGen.genUnit(statistics, unit, generatedHandler)
    }
    def lazyStats(s:String) = {
      for (k <- bTypes.Lazy.lazyCount.keys) {
        println(s"LAZY STATS - ${s} $k ${bTypes.Lazy.lazyCount(k)}/${bTypes.Lazy.lazyForced(k)}")

      }
    }

    override def run(): Unit = {
      statistics.timed(bcodeTimer) {
        try {
          bTypes.Lazy.lazyCount.clear()
          bTypes.Lazy.lazyForced.clear()
          initialize()
          lazyStats("1")
          super.run() // invokes `apply` for each compilation unit
          lazyStats("2")
          generatedHandler.complete()
          lazyStats("3")
        } catch {
          case t:Throwable =>
            t.printStackTrace()
        } finally {
          // When writing to a jar, we need to close the jarWriter.
          generatedHandler.close()
          lazyStats("4")
        }
      }
    }

    /**
     * Several backend components have state that needs to be initialized in each run, because
     * it depends on frontend data that may change between runs: Symbols, Types, Settings.
     */
    private def initialize(): Unit = {
      val initStart = statistics.startTimer(bcodeInitTimer)
      scalaPrimitives.init()
      bTypes.initialize()
      codeGen.initialize()
      postProcessorFrontendAccess.initialize()
      postProcessor.initialize()
      val asyncHelper = new AsyncHandler(global, this, "")
      val cfWriter = ClassfileWriter(asyncHelper, global.cleanup, settings, statistics, postProcessorFrontendAccess )
      generatedHandler = ClassHandler(asyncHelper, cfWriter, settings, postProcessor)
      statistics.stopTimer(statistics.bcodeInitTimer, initStart)
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
