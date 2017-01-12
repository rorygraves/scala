package scala.tools.linkertool

import java.io.File

import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option => ArgsjOption}


/**
  * @author Mike Skells
  */
object LinkerToolCommandLine{

  //basic options
  @ArgsjOption(name="-in", aliases=Array("--input"), required = true,
    usage="input jar file to process. Exactly one of -in or -inPath must be specified"  )
  private var inputFileV:File = _
  def inputFile = inputFileV

  @ArgsjOption(name="-cp", aliases=Array("--classPath"),
    usage="classPath to use. Default is the same as -in"  )
  private var classPathV:File = _
  def classPath = scala.Option(classPathV).getOrElse(inputFile)

  @ArgsjOption(name="-out", aliases=Array("--output"),
    usage="output jar file to process - default is to use the same name as the input filename, but in the local directory. " +
      "If the input is a path, then the output should be a directory, and the files will have names assigned based on the input files names"  )
  private var outputFileV:File = _
  lazy val outputFile = scala.Option(outputFileV).getOrElse(new File(inputFile.getName))

  @ArgsjOption(name="-over",aliases=Array("--overwriteOutput"),
    usage="overwrite the output file if it exists"  )
  private var overwriteV:Boolean = _
  def overwrite = overwriteV


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
  @ArgsjOption(name="-debug", usage="dump to the colsole all of the symbols"  )
  private var debugV:Boolean = _
  def debug = debugV


  //  @ArgsjOption(name="-jp", aliases=Array("--javap"),
//    usage="run javap on some of the result"  )
//  private var javapV:String = _
//  def javaP = javapV
//



  val parser = new CmdLineParser(this)
  parser.getProperties.withShowDefaults(true).withUsageWidth(120)

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

    require(inputFile.exists, s"input $inputFile doesn't exist")
    require(inputFile.isFile, s"input $inputFile isn't a file")
    require(overwrite || !outputFile.exists, s"output $outputFile exists")

  }
}
