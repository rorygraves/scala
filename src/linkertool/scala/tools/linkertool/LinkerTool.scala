package scala.tools.linkertool

import java.io._
import java.nio.file._
import java.util.jar._
import java.util.zip._

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters._
import scala.collection.JavaConversions._
import scala.collection.breakOut
import scala.tools.linker._

/**
  * @author Mike Skells
  */
object LinkerTool extends App {

  case class JarClassInfo(val entryName: String, val packageName: String, val symbolName: String) {
    def className = if (packageName.isEmpty) symbolName else s"$packageName.$symbolName"
  }

  import LinkerToolCommandLine._


  def error(s: String): Unit = {
    sys.error(s)
    sys.exit(1)
  }

  init(args)

  val inJar = new JarFile(inputFile)
  val entries: List[JarClassInfo] = inJar.entries.toList.map {
    jarEntry =>
      val name = jarEntry.getName
      val dirIndex = name.lastIndexOf('/')
      val packageName = if (dirIndex == -1) "" else name.substring(0, dirIndex).replace('/', '.')
      val localName = name.substring(dirIndex + 1)
      val dollarIndex = localName.indexOf('$')
      val symbolName = if (dollarIndex == -1) localName else localName.substring(0, dollarIndex)
      JarClassInfo(name, packageName, symbolName)
  }

  val byName = entries.groupBy(_.className)

  val reporter = new StoreReporter


  val global = new Global(new Settings(x => sys.error(x)), reporter)
  new global.Run()

  import global._, global.definitions._

  byName foreach {
    case (name, entries) =>
      val dummyUnit = newCompilationUnit("", "dummy.scala")
      val javaSym = rootMirror.getClassIfDefined(name)
      dummyUnit.body = ClassDef(javaSym, NoMods, Nil, Nil, NoPosition)
      currentRun.symSource(javaSym) = dummyUnit.source.file
      currentRun.picklerPhase.asInstanceOf[GlobalPhase].apply(dummyUnit)
  }
  currentRun.linkerPhase.run()

  val data = currentRun.linkerData.getOrElse(???)

  if (overwrite) Files.copy(inputFile.toPath, outputFile.toPath, StandardCopyOption.REPLACE_EXISTING)
  else Files.copy(inputFile.toPath, outputFile.toPath)

  val fs = FileSystems.newFileSystem(outputFile.toPath, getClass.getClassLoader)

  val extra = (LinkerSymbols.fileName(true), data.toBytes(true)) :: //
    (LinkerSymbols.fileName(false), data.toBytes(false)) :: Nil

  for ((name, data) <- extra) {
    val path = fs.getPath(name)
    Files.createDirectories(path.getParent)
    Files.copy(new ByteArrayInputStream(data), fs.getPath(name))
  }
  fs.close()


  if (debug) {
    val global = new Global(new Settings(x => sys.error(x)), reporter)
    new global.Run()
    import global._, global.definitions._
    object unpickler extends scala.reflect.internal.pickling.UnPickler {
      val symbolTable: global.type = global
    }
    byName foreach {
      case (name, entries) =>
        val first = entries.head
        println(s"$name -> $entries")
        val pack = rootMirror.getPackageIfDefined(TermName(first.packageName)).moduleClass
        val (moduleSym, classSym) = pack.newModuleAndClassSymbol(TermName(first.symbolName), NoPosition, 0L)
        // TODO enter symbols in info of package class.

        val sig = data.allClasses(name) match {
          case ref : ScalaClassReference => ref.scalaClassSignature
        }

        unpickler.unpickle(sig.scalaSigBytesCopy, 0, classSym, moduleSym, "Dummy.file")
        definitions.fullyInitializeSymbol(classSym)
        println(s"Start of $name")
        dump(classSym, "")
        dump(classSym.companion, "  ")
        println(s"end of $name")
        classSym.info.decls.foreach(x => println(fullyInitializeSymbol(x).defString))
        assert(classSym.isJavaDefined)


    }
    def dump(sym:Symbol, indent:String): Unit = {
      definitions.fullyInitializeSymbol(sym)
      println(s"java?${sym.isJavaDefined}$indent  ${sym.defString}")
      sym.info.decls.foreach(x => dump(x,indent+"  "))
    }
//
//    val pack = rootMirror.getPackageIfDefined(TermName("java.util")).moduleClass
//    val (moduleSym, classSym) = pack.newModuleAndClassSymbol(TermName("Random"), NoPosition, 0L)
//
//    unpickler.unpickle(pickleData, 0, classSym, moduleSym, "Dummy.java")
//    definitions.fullyInitializeSymbol(classSym)
//    classSym.info.decls.foreach(x => println(fullyInitializeSymbol(x).defString))
//    assert(classSym.isJavaDefined)
  }

}


