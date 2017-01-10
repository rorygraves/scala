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

  val phaseName = "linker"

  def newPhase(prev: Phase): Phase = new LinkerPhase(prev)

  class LinkerPhase (prev: Phase) extends global.GlobalPhase(prev) {
    def name = phaseName

    override def run(): Unit = {
      val enabled :Boolean = global.settings.linker
      println(s"linker enabled:$enabled for ${currentRun.symData.size}")
      if (global.settings.debug) inform("[phase " + name + " - enabled: "+enabled + "]")
      if (enabled) {
        val visited = new mutable.HashSet[Symbol]()
        //TODO linkerData should b synced with current classes for incremental compilation
        val linkerData = new RootSymbolWriter
        currentRun.symData foreach {
          //only process elements and companion pairs once
          case (sym, pickleBuffer) => if (visited.add(sym)) {
            if (currentRun.symData.contains(sym.companion)) {
              println(s"sym $sym, companion ${sym.companion} companion^2 ${sym.companion.companion} .. ${sym.name.toString}")
              println(s"sym $sym, companionSymbol ${sym.companionSymbol} companionSymbol^2 ${sym.companionSymbol.companionSymbol} .. ${sym.name.toString}")
              assert(sym.companion.companion eq sym)
              assert(sym.companion eq sym.companionSymbol)
              assert(sym.companionSymbol.companionSymbol eq sym)
              assert(currentRun.symData(sym.companion) eq pickleBuffer)
              visited.add(sym.companion)
            }
            //not sure if this is right - do we want the BinaryName or the Class name etc
            val binaryClassName = sym match {
              case c:ClassSymbol => sym.javaBinaryNameString
              case m:ModuleSymbol if currentRun.symData.contains(sym.companion) => sym.companion.javaBinaryNameString
              case m:ModuleSymbol => sym.javaBinaryNameString
            }

            println(s"linker - add $binaryClassName")
            linkerData.addClassRef(ScalaLinkerClassInfo(binaryClassName, pickleBuffer))
          }
        }
        val sym = global.symbolOf[java.util.Collections]
        val raw = ScalaClassSignature(global.pickler.pickle(sym))

        println(s"linker - pickled collections ${raw}   ")

        //TODO : consider use a Future?
        currentRun.linkerData = Some(linkerData)
      }
    }

    //apply should not be called
    def apply(unit: CompilationUnit): Unit = ???

  }
  object ScalaLinkerClassInfo {
    def apply(binaryClassName:String, pickleBuffer:PickleBuffer): ScalaLinkerClassInfo = {
      val signature = ScalaClassSignature(pickleBuffer)
      new ScalaLinkerClassInfo(binaryClassName, signature)
    }
  }
  case class ScalaLinkerClassInfo(binaryName:String, sig:ScalaClassSignature) extends ClassInfo {
    override def javaClassName: String = binaryName.replace('/','.')

    override def internalClassName: String = binaryName

    override def outerJavaClassName: Option[String] = None

    override def scalaSignature: Option[ScalaClassSignature] = Some(sig)

    override def entryName: String = binaryName + ".class"
  }

  def getFiles : Map[String,Array[Byte]] = {
    currentRun.linkerData match {
      case None =>
        assert(!global.settings.linker)
        Map.empty
      case Some(linkerData) =>
        Map(
          RootSymbolWriter.fileName(true) -> linkerData.toBytes(true),
          RootSymbolWriter.fileName(false)-> linkerData.toBytes(false)
        )
    }
  }
}
