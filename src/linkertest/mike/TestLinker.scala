package mike

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.{ConsoleReporter, StoreReporter}

/**
  * Created by dev on 15/05/2017.
  */
object TestLinker extends App {
 val params = new TestCmdLine(args)

  def newGlobal() = {
    val settings = new Settings(x => sys.error(x))
    settings.classpath.append(params.classPath)

    val reporter = new ConsoleReporter(settings)
    val global = new Global(settings, reporter)
    new global.Run()

    global
  }
  val global = newGlobal()
  import global._, global.definitions._

  global.settings.linker.value = true
  global.settings.d.value = params.outputFile.getCanonicalPath
  global.settings.verbose.value = params.verbose

  global.currentRun.compile(List("S:\\scala\\scala_perf\\src\\linkertest\\mike\\TestAkka.scala"))

}
