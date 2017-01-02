package scala.tools.nsc

import scala.collection.mutable
import scala.reflect.internal.pickling.PickleBuffer
import scala.tools.linker.{ClassInfo, RootSymbolWriter, ScalaClassSignature}


/**
  * Create Linker information for the compiled code
  * The linker info contains the minimal data set to compile a dependent module
  *
  * @author Mike Skells
  * @author Rory Graves
  * @version 1.0
  */
abstract class Linker  extends SubComponent {
  import global._

  val phaseName = "pickler"

  def newPhase(prev: Phase): Phase = new LinkerPhase(prev)

  class LinkerPhase (prev: Phase) extends global.GlobalPhase(prev) {
    def name = phaseName

    override def run(): Unit = {
      val enabled :Boolean = global.settings.linker
      if (global.settings.debug) inform("[phase " + name + " - enabled: "+enabled + "]")
      if (enabled) {
        val visited = new mutable.HashSet[Symbol]()
        val linkerData = new RootSymbolWriter
        currentRun.symData foreach {
          //only process elements and companion pairs once
          case (sym, pickleBuffer) if visited.add(sym) =>
            if (currentRun.symData.contains(sym.companion)) {
              assert(currentRun.symData(sym.companion) eq pickleBuffer)
              visited.add(sym.companion)
            }
            //not sure if this is right
            val name = sym match {
              case m: ModuleSymbol => m.name.toString
              case c: ClassSymbol => c.name.toString
            }

            linkerData.addClassRef(ScalaLinkerClassInfo(name, pickleBuffer))
        }
      }
    }

    //apply should not be called
    def apply(unit: CompilationUnit): Unit = ???

  }
  object ScalaLinkerClassInfo {
    def apply(name:String, pickleBuffer:PickleBuffer): ScalaLinkerClassInfo = {
      val signature = ScalaClassSignature(pickleBuffer)
      new ScalaLinkerClassInfo(name, signature)
    }
  }
  case class ScalaLinkerClassInfo(name:String, sig:ScalaClassSignature) extends ClassInfo {
    override def javaClassName: String = name

    override def internalClassName: String = name.replace('.','/')

    override def outerJavaClassName: Option[String] = None

    override def scalaSignature: Option[ScalaClassSignature] = Some(sig)

    override def entryName: String = ???
  }
}
