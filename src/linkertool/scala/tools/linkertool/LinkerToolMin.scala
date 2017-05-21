//package scala.tools.linkertool.xx
//
//import java.util.jar._
//
//import scala.collection.JavaConversions._
//import scala.tools.nsc.reporters._
//import scala.tools.nsc.{Global, Settings}
//
///**
//  * @author Mike Skells
//  */
//object LinkerToolMin extends App {
//  case class JarClassInfo(val entryName: String, val packageName: String, val localName: String) {
//    def className = if (packageName.isEmpty) localName else s"$packageName.$localName"
//  }
//
//  def newGlobal(includeInput:Boolean, console:Boolean) = {
//    val settings = new Settings(x => sys.error(x))
//    settings.classpath.append("C:\\Users\\dev\\.m2\\repository\\com\\typesafe\\akka\\akka-actor_2.12\\2.5.1\\akka-actor_2.12-2.5.1.jar")
//    settings.classpath.append("C:\\Users\\dev\\.m2\\repository\\com\\typesafe\\config\\1.3.1\\config-1.3.1.jar")
//    settings.classpath.append("C:\\Users\\dev\\.m2\\repository\\org\\scala-lang\\modules\\scala-java8-compat_2.12\\0.8.0\\scala-java8-compat_2.12-0.8.0.jar")
//    settings.classpath.append("C:\\Users\\dev\\.m2\\repository\\org\\scala-lang\\scala-library\\2.12.2\\scala-library-2.12.2.jar")
//
//    settings.usejavacp.value=true
//    val reporter = if (console) new ConsoleReporter(settings) else new StoreReporter
//    val global = new Global(settings, reporter)
//    new global.Run()
//
//    global
//  }
//  var entries : List[JarClassInfo] = {
//    val inJar = new JarFile("C:\\Users\\dev\\.m2\\repository\\com\\typesafe\\akka\\akka-actor_2.12\\2.5.1\\akka-actor_2.12-2.5.1.jar")
//    val res = inJar.entries.toList.filter(_.getName.endsWith(".class")).map {
//      jarEntry =>
//        val name = jarEntry.getName
//        val dirIndex = name.lastIndexOf('/')
//        val packageName = if (dirIndex == -1) "" else name.substring(0, dirIndex).replace('/', '.')
//        val localName = name.substring(dirIndex + 1)
//        JarClassInfo(name, packageName, localName.take(localName.length - 6))
//    }
//    inJar.close()
//    res
//
//  }
//
//
//  def getReadableSymbols() = {
//    val global = newGlobal(true, true)
//    import global._, global.definitions._
//
//    val withSym = global.enteringTyper {
//      entries.collect {
//        case entry@JarClassInfo(entryName, packageName, symbolName) if entryName contains "StashWhenFailed" =>
//          val cls = rootMirror.getRequiredClass(entry.className)
//          println(s"cls $cls ${cls.hashCode}")
//          println(s"companion ${cls.companion} ${cls.companion.hashCode}")
//          println(s"companion2 ${cls.companion.companion} ${cls.companion.companion.hashCode}")
//
//          val mod = rootMirror.getRequiredModule(entry.className)
//          println(s"mod $mod ${mod.hashCode}")
//          println(s"companion ${mod.companion} ${mod.companion.hashCode()}")
//          println(s"companion2 ${mod.companion.companion} ${mod.companion.companion.hashCode}")
//          println(cls.exists)
//          println(mod.exists)
//
//          (entry, cls)
//      }
//    }
//    global.enteringPickler {
//      withSym map {
//        case (entry, sym) =>
//          global.pickler.pickle(true, sym, sym.companion)
//      }
//    }
//    currentRun.symData foreach {
//      //only process elements and companion pairs once
//      case (sym, pickleBuffer) => if (sym != NoSymbol) {
//        println(s"processing $sym ${sym.hashCode} $pickleBuffer")
//        println(s"companion ${sym.companion}")
//        println(s"companion2 ${sym.companion.companion}")
//      }
//    }
//  }
//  val linkerData = getReadableSymbols()
//
//
//
//}
//
//
