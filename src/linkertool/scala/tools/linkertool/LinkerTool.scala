package scala.tools.linkertool

import java.io._
import java.nio.file._
import java.util.jar._
import java.util.zip._

import scala.annotation.tailrec
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters._
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.breakOut
import scala.tools.linker._
import scala.tools.nsc.symtab.classfile.Pickler
import scala.util.control.NonFatal

/**
  * @author Mike Skells
  */
object LinkerTool extends App {


  case class JarClassInfo(val entryName: String, val packageName: String, val localName: String, parent:String) {
    def className = if (packageName.isEmpty) localName else s"$packageName.$localName"
//    def outmostSymbolName = {
//      val dIndex = symbolName.indexOf('$')
//      if (dIndex == -1) symbolName else symbolName.take(dIndex)
//    }
//    def outmostClassName = {
//      if (packageName.isEmpty) outmostSymbolName else s"$packageName.$outmostSymbolName"
//    }
    def localSymbolName = localName.replace('$','.').replace("..",".")
    def fullSymbolName = if (packageName.isEmpty) localSymbolName else s"$packageName.$localSymbolName"
  }

  private def error(s: String): Unit = {
    sys.error(s)
    sys.exit(1)
  }

  val params = new LinkerToolCommandLine(args)

  @inline def verbose (txt: => String): Unit = {
    if (params.verbose) println(txt)
  }

  val entries: List[JarClassInfo] = {
    val inJar = new JarFile(params.inputFile)
    val res = inJar.entries.toList.filter( _.getName.endsWith(".class")).map {
      jarEntry =>
        val name = jarEntry.getName
        val dirIndex = name.lastIndexOf('/')
        val packageName = if (dirIndex == -1) "" else name.substring(0, dirIndex).replace('/', '.')
        val localName = name.substring(dirIndex + 1)
//        val dollarIndex = localName.indexOf('$')
//        //name ends with ".class" or "$<something"
//        val symbolName = localName.take(if (dollarIndex == -1) localName.length - 6 else dollarIndex )
        JarClassInfo(name, packageName, localName.take(localName.length -6), "")
    }
    inJar.close()
    val byName = res.groupBy(_.className)
    @tailrec def findParentClassName(className:String): String = {
      val di =className.lastIndexOf('$')
      if (di == -1) ""
      else {
        val parent = className.take(di)
        if (byName.contains(parent)) parent
        else findParentClassName(parent)
      }
    }
    res map {
      case j @ JarClassInfo(name, packageName, localName, _ ) =>
        JarClassInfo(name, packageName, localName, findParentClassName(j.className) )
    }
  }

  def newGlobal(includeInput:Boolean, console:Boolean) = {
    val settings = new Settings(x => sys.error(x))
    val reporter = if (console) new ConsoleReporter(settings) else new StoreReporter
    val global = new Global(settings, reporter)
    new global.Run()

    global
  }

  val allBadSymbols = new mutable.HashSet[String]()
//  val allBadEntries = new mutable.HashSet[String]()

  val topLevel: List[JarClassInfo] = entries filter (_.parent == "")
//    val global = newGlobal(true, true)
//    global.settings.debug.value = true
//    import global._
//
//    val toTopSym = entries.
////    filterNot {
////      case JarClassInfo(entryName, _, _) => allBadEntries.contains(entryName)
////    }
//    map {
//      case entry :JarClassInfo =>
//        verbose(s" loading ${entry.entryName} ")
//        verbose(s" loading ${entry.fullSymbolName} ")
//        val sym = rootMirror.getRequiredClass(entry.fullSymbolName)
//        (entry, sym, sym.enclosingTopLevelClass)
//    }
//    val topLevel:Map[Symbol, JarClassInfo] = toTopSym.collect{
//      case (entry, sym, top) if sym == top=> (top->entry)
//    } .toMap
//    toTopSym.groupBy {
//      case (entry, sym, top) => topLevel(top)
//    }.map {
//      case (k,list) => (k-> list.map(_._1))
//    }
//  }

  @tailrec def getReadableSymbols() :RootLinkerSymbolWriter= {
    val initialSize = allBadSymbols.size
    val global = newGlobal(true, false)
    import global._, global.definitions._

    val withSym = topLevel.filter {
      case JarClassInfo(entryName, _,_,parent) => parent == "" && !allBadSymbols.contains(entryName)
    } map {
      case entry @ JarClassInfo(entryName, packageName, symbolName, parent) =>
        verbose(s" loading $entryName ")
//        verbose(s" loading ${entry.fullSymbolName} ")
        (entry, rootMirror.getRequiredClass(entry.className))
    }
    withSym map {
      case (entry, sym) =>
        //        val dummyUnit = newCompilationUnit("", "dummy.scala")
        //        dummyUnit.body = ClassDef(sym, NoMods, Nil, Nil, NoPosition)
        //        currentRun.symSource(sym) = dummyUnit.source.file
        //        currentRun.picklerPhase.asInstanceOf[GlobalPhase].apply(dummyUnit)
        try global.pickler.pickle(true, sym, sym.companion)
        catch {
          case NonFatal(x) =>
            if (params.verbose) x.printStackTrace
            allBadSymbols += entry.entryName
        }
    }
    if (initialSize != allBadSymbols.size) {
      if (params.verbose) inform(s"${allBadSymbols.size} bad symbols detected - retrying")
      getReadableSymbols()
    } else {
      //val symData = currentRun.symData.toMap
      currentRun.linkerPhase.run()
      currentRun.linkerData.getOrElse(???)
    }
  }
  val linkerData = getReadableSymbols()


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
    val global = newGlobal(false, false)
    import global._
    import global.definitions._
    import scala.collection.mutable


    object unpickler extends scala.reflect.internal.pickling.UnPickler {
      val symbolTable: global.type = global

      var unpickledSymbols: mutable.HashSet[Symbol] = _

      override def newScan(_bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) = {
        require (unpickledSymbols == null)
        unpickledSymbols = new mutable.HashSet[Symbol]()
        println("new TraceScan")
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

    val classAndData = topLevel.filter { jar => !allBadSymbols.contains(jar.entryName)} map { top =>
        verbose(s"$top -> $top")
        val pack = rootMirror.getPackageIfDefined(TermName(top.packageName)).moduleClass
        val (moduleSym, classSym) = pack.newModuleAndClassSymbol(TermName(top.localSymbolName), NoPosition, 0L)
        // TODO enter symbols in info of package class.

        val sig = linkerData.allClasses(top.className) match {
          case ref: ScalaClassReference => ref.scalaClassSignature
        }
        (top, moduleSym, classSym, sig)
    }
    val unpickledSymbols = classAndData map {
      case (name, moduleSym, classSym, sig) =>
        unpickler.unpickle(sig.scalaSigBytesCopy, 0, classSym, moduleSym, "Dummy.file")
        val unpickled = unpickler.unpickledSymbols.toSet
        unpickler.unpickledSymbols = null
        (name, moduleSym, classSym, unpickled)
    }
    unpickledSymbols foreach {
      case (name, moduleSym, classSym, unpickled) =>

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


