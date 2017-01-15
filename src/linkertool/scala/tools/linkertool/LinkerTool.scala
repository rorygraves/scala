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
    def outmostSymbolName = {
      val dIndex = symbolName.indexOf('$')
      if (dIndex == -1) symbolName else symbolName.take(dIndex)
    }
    def outmostClassName = {
      if (packageName.isEmpty) outmostSymbolName else s"$packageName.$outmostSymbolName"
    }
  }

  private def error(s: String): Unit = {
    sys.error(s)
    sys.exit(1)
  }

  val params = new LinkerToolCommandLine(args)

  val entries: List[JarClassInfo] = {
    val inJar = new JarFile(params.inputFile)
    val res = inJar.entries.toList.filter( _.getName.endsWith(".class")).map {
      jarEntry =>
        val name = jarEntry.getName
        val dirIndex = name.lastIndexOf('/')
        val packageName = if (dirIndex == -1) "" else name.substring(0, dirIndex).replace('/', '.')
        val localName = name.substring(dirIndex + 1)
        val dollarIndex = localName.indexOf('$')
        //name ends with ".class" or "$<something"
        val symbolName = localName.take(if (dollarIndex == -1) localName.length - 6 else dollarIndex )
        JarClassInfo(name, packageName, symbolName)
    }
    inJar.close()
    res
  }

  val byOutMostName = entries.groupBy(_.outmostClassName)

  def newGlobal(includeInput:Boolean) = {
    val reporter = new StoreReporter
    val global = new Global(new Settings(x => sys.error(x)), reporter)
    new global.Run()

    global
  }

  val linkerData = {
    val global = newGlobal(true)
    import global._, global.definitions._

    var count = 1
    byOutMostName foreach {
      case (name, entries) =>
        println(s"count $count - $name $entries")
        count += 1
        val dummyUnit = newCompilationUnit("", "dummy.scala")
        val javaSym = rootMirror.getClassIfDefined(name)
        dummyUnit.body = ClassDef(javaSym, NoMods, Nil, Nil, NoPosition)
        currentRun.symSource(javaSym) = dummyUnit.source.file
        currentRun.picklerPhase.asInstanceOf[GlobalPhase].apply(dummyUnit)
    }
    currentRun.linkerPhase.run()
    currentRun.linkerData.getOrElse(???)
  }



  //copy and add new entries to the jar
  if (params.overwrite) Files.copy(params.inputFile.toPath, params.outputFile.toPath, StandardCopyOption.REPLACE_EXISTING)
  else Files.copy(params.inputFile.toPath, params.outputFile.toPath)

  addRefs
  def addRefs() = {
    val fs = FileSystems.newFileSystem(params.outputFile.toPath, getClass.getClassLoader)

    val extra = (LinkerSymbols.fileName(true), linkerData.toBytes(true)) :: //
      (LinkerSymbols.fileName(false), linkerData.toBytes(false)) :: Nil

    for ((name, data) <- extra) {
      val path = fs.getPath(name)
      Files.createDirectories(path.getParent)
      Files.copy(new ByteArrayInputStream(data), fs.getPath(name))
    }
    fs.close()
  }


  if (params.debug) {
    val global = newGlobal(false)
    import global._
    import global.definitions._
    import scala.collection.mutable


    object unpickler extends scala.reflect.internal.pickling.UnPickler {
      val symbolTable: global.type = global

      var unpickledSymbols: mutable.HashSet[Symbol] = _

      override def newScan(_bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) = {
        require (unpickledSymbols == null)
        unpickledSymbols = new mutable.HashSet[Symbol]
        new TraceScan(_bytes, offset, classRoot, moduleRoot, filename)
      }

      class TraceScan(_bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) extends Scan(_bytes, offset, classRoot, moduleRoot, filename) {
        override protected def readSymbol(): unpickler.symbolTable.Symbol = {

          var r = super.readSymbol()
          unpickledSymbols += r
          r
        }
      }
    }

    println(linkerData.allClasses.keySet)
    val classAndData = byOutMostName map {
      case (name, entries) =>
        val first = entries.head
        println(s"$name -> $entries")
        val pack = rootMirror.getPackageIfDefined(TermName(first.packageName)).moduleClass
        val (moduleSym, classSym) = pack.newModuleAndClassSymbol(TermName(first.outmostSymbolName), NoPosition, 0L)
        // TODO enter symbols in info of package class.

        val sig = linkerData.allClasses(first.className) match {
          case ref: ScalaClassReference => ref.scalaClassSignature
        }
        (name, entries, moduleSym, classSym, sig)
    }
    val unpickledSymbols = classAndData map {
      case (name, entries, moduleSym, classSym, sig) =>
        unpickler.unpickle(sig.scalaSigBytesCopy, 0, classSym, moduleSym, "Dummy.file")
        val unpickled = unpickler.unpickledSymbols.toSet
        unpickler.unpickledSymbols = null
        (name, entries, moduleSym, classSym, unpickled)
    }
    unpickledSymbols foreach {
      case (name, entries, moduleSym, classSym, unpickled) =>

//        definitions.fullyInitializeSymbol(classSym)
        val visited = new mutable.HashSet[Symbol]
        println(s"Start of $name")
        dump(classSym, "", unpickled, visited)
        dump(classSym.companion, "  ", unpickled, visited)
        println(s"end of $name")
        require (visited == unpickled)
//        classSym.info.decls.foreach(x => println(fullyInitializeSymbol(x).defString))
//        assert(classSym.isJavaDefined)


    }
    def dump(sym:Symbol, indent:String, allSymbolsToConsider: Set[Symbol], visited : mutable.HashSet[Symbol]): Unit = {
      if (allSymbolsToConsider.contains(sym) && visited.add(sym)) {

        println(s"java?${sym.isJavaDefined}$indent  ${sym.defString}")
//        definitions.fullyInitializeSymbol(sym)
//        println(s"java?${sym.isJavaDefined}$indent  ${sym.defString}")
        sym.info.decls.foreach(x => dump(x, indent + "  ", allSymbolsToConsider, visited))
      }
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


