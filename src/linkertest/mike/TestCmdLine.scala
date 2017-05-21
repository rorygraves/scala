package mike

import java.io.File

import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option => ArgsjOption}


/**
  * @author Mike Skells
  */
class TestCmdLine(args: Array[String]){

  val parser = new CmdLineParser(this)
  parser.getProperties.withShowDefaults(true).withUsageWidth(120)

  init(args)
  @ArgsjOption(name="-cp", aliases=Array("--classPath"), required = true,
    usage="classPath to use. Required"  )
  private var classPathV:String = _
  def classPath = classPathV


  @ArgsjOption(name="-out", aliases=Array("--output"), required = true,
    usage="output dir. Required"
  )
  private var outputFileV:File = _
  lazy val outputFile = outputFileV

  //admin
  @ArgsjOption(name="-h", aliases=Array("-?", "--help", "-help"),
    usage="print help"  )
  private var helpV:Boolean = _
  def help = helpV

  @ArgsjOption(name="-v", aliases=Array("--verbose"),
    usage="be verbose"  )
  private var verboseV:Boolean = _
  def verbose = verboseV

  //debugging
  @ArgsjOption(name="-debug", usage="dump to the console all of the symbols"  )
  private var debugV:Boolean = _
  def debug = debugV

  def parametersInError(msg:String): Nothing = {
    println(msg)
    displayHelp(1)
  }
  private def displayHelp(exitCode: Int) :Nothing= {
    parser.printUsage(System.out)
    sys.exit(exitCode)
  }
  def init(args:Array[String]): Unit = {
    try parser.parseArgument(args: _*) catch {
      case e: CmdLineException =>
        if (help) {
          displayHelp(0)
        } else {
          parametersInError(e.getMessage)
        }
    }

  }
}
