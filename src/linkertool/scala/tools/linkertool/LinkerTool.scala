package scala.tools.linkertool

import java.io._
import java.nio.file._
import java.util.jar._
import java.util.zip._

import scala.annotation.tailrec
import scala.tools.nsc.{Global, ScalaLinkerClassInfo, Settings}
import scala.tools.nsc.reporters._
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.breakOut
import scala.reflect.internal.SymbolTable
import scala.reflect.internal.pickling.PickleBuffer
import scala.tools.linker._
import scala.tools.nsc.symtab.classfile.Pickler
import scala.util.control.NonFatal

/**
  * @author Mike Skells
  */
object LinkerTool extends App {

  case class JarClassInfo(val entryName: String, val packageName: String, val localName: String, parent:String) {
    def className = if (packageName.isEmpty) localName else s"$packageName.$localName"
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
    settings.classpath.append(params.classPath)
    if (includeInput)
      settings.classpath.prepend(params.inputFile.getAbsolutePath)

    settings.usejavacp.value=params.useJavaClassPath
    val reporter = if (console) new ConsoleReporter(settings) else new StoreReporter
    val global = new Global(settings, reporter)
    new global.Run()

    global
  }

  val topLevel: List[JarClassInfo] = entries filter (_.parent == "")


  def getReadableSymbols() :RootLinkerSymbolWriter= {
    val global = newGlobal(true, true)
    import global._, global.definitions._

    case class EntrySymbol(jarInfo: JarClassInfo, cls: Option[Symbol], mod: Option[Symbol]) {
      val root = cls.getOrElse((mod.getOrElse(???)))

      def otherSymbol = if (cls.isDefined) mod else None
    }
    case class PickleSymbol(entry: EntrySymbol, pickle: PickleBuffer)

    def ifExists(sym: Symbol) = if (sym.exists) Some(sym) else None

    val withSym = topLevel.collect {
      case entry@JarClassInfo(entryName, packageName, symbolName, "") =>
        val cls = ifExists(rootMirror.getClassIfDefined(entry.className))
        val mod = ifExists(rootMirror.getModuleIfDefined(entry.className))
        EntrySymbol(entry, cls, mod)
    }
    val withPickle = withSym map {
      case entrySym@EntrySymbol(entry, cls, mod) =>
        PickleSymbol(entrySym, global.pickler.pickle(true, entrySym.root, entrySym.otherSymbol))
    }
    val linkerData = new RootLinkerSymbolWriter
    withPickle foreach {
      case PickleSymbol(entry, pickle) =>
        linkerData.addClassRef(ScalaLinkerClassInfo(entry.root.javaBinaryNameString, pickle))
    }
    linkerData
  }
  val linkerData = getReadableSymbols()


  //copy and add new entries to the jar
  verbose(s"adding ${linkerData.count} symbols")
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


//  if (params.debug) {
//    val global = newGlobal(false, false)
//    import global._
//    import global.definitions._
//    import scala.collection.mutable
//
//    println(s"${linkerData.allClasses.keys.toList.sorted.mkString("\n")}")
//
//    object unpickler extends scala.reflect.internal.pickling.UnPickler {
//      val symbolTable: global.type = global
//
//      var unpickledSymbols = new mutable.HashSet[Symbol]
//
//      override def newScan(_bytes: Array[Byte], offset: Int, classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String) = {
//        new TraceScan(_bytes, offset, classRoot, moduleRoot, filename)
//      }
//
//      class TraceScan(_bytes: Array[Byte], offset: Int, classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String) extends Scan(_bytes, offset, classRoot, moduleRoot, filename) {
//        override protected def readSymbol(): unpickler.symbolTable.Symbol = {
//          var r = super.readSymbol()
//          unpickledSymbols += r
//          r
//        }
//      }
//
//    }
//
//    val classAndData = topLevel.filter { jar => !allBadSymbols.contains(jar.entryName) } map { top =>
//      verbose(s"$top -> $top")
//      val pack = rootMirror.getPackageIfDefined(TermName(top.packageName)).moduleClass
//      val (moduleSym, classSym) = pack.newModuleAndClassSymbol(TermName(top.localSymbolName), NoPosition, 0L)
//      // TODO enter symbols in info of package class.
//
//      val sig = linkerData.allClasses.get(top.className) match {
//        case Some(ref: ScalaClassReference) => ref.scalaClassSignature
//        case None => println("*** top.className "+top.className); null
//      }
//      (top, moduleSym, classSym, sig)
//    }
//    val unpickledSymbols = classAndData.filter (_._4 ne null) map {
//      case (name, moduleSym, classSym, sig) =>
//        unpickler.unpickle(sig.scalaSigBytesCopy, 0, classSym, moduleSym, "Dummy.file")
//        val unpickled = unpickler.unpickledSymbols.toSet
//        unpickler.unpickledSymbols.clear
//        (name, moduleSym, classSym, unpickled)
//    }
//    unpickledSymbols foreach {
//      case (name, moduleSym, classSym, unpickled) =>
//
//        //        definitions.fullyInitializeSymbol(classSym)
//        val visited = new mutable.HashSet[Symbol]
//        println(s"Start of $name")
//        dump(classSym, "", unpickled, visited, true)
//        dump(classSym.companion, "  ", unpickled, visited, true)
//        println(s"end of $name")
////        require(visited.toSet == unpickled)
//      //        classSym.info.decls.foreach(x => println(fullyInitializeSymbol(x).defString))
//      //        assert(classSym.isJavaDefined)
//
//
//    }
//
//    def dump(sym: Symbol, indent: String, allSymbolsToConsider: Set[Symbol], visited: mutable.HashSet[Symbol], isRoot:Boolean): Unit = {
//      if (isRoot || (allSymbolsToConsider.contains(sym) && visited.add(sym))) {
//        sym.initialize
//
//        //        definitions.fullyInitializeSymbol(sym)
//        //        println(s"java?${sym.isJavaDefined}$indent  ${sym.defString}")
//        sym match {
//          case s if s.isLazy =>
//            println(s"java?${sym.isJavaDefined}$indent  ${sym}  *** TODO LAZY")
//          case s if s.isMethod=>
//            println(s"java?${sym.isJavaDefined}$indent  ${sym}  *** TODO method")
//          case _ =>
//            println(s"java?${sym.isJavaDefined}$indent  ${sym.defString}")
//            sym.info.decls.foreach(x => dump(x, indent + "  ", allSymbolsToConsider, visited, false))
//        }
//      }
//    }
//
//    //
//    //    val pack = rootMirror.getPackageIfDefined(TermName("java.util")).moduleClass
//    //    val (moduleSym, classSym) = pack.newModuleAndClassSymbol(TermName("Random"), NoPosition, 0L)
//    //
//    //    unpickler.unpickle(pickleData, 0, classSym, moduleSym, "Dummy.java")
//    //    definitions.fullyInitializeSymbol(classSym)
//    //    classSym.info.decls.foreach(x => println(fullyInitializeSymbol(x).defString))
//    //    assert(classSym.isJavaDefined)
//  }
}


