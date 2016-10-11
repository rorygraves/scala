package scala.tools.jarshrink

import java.io.File

import org.kohsuke.args4j.spi.StringArrayOptionHandler
import org.kohsuke.args4j.{CmdLineException, CmdLineParser}

import scala.util.matching.Regex


/**
  * Created by User on 05/10/2016.
  */
object JarShrinkCommandLine{
  import org.kohsuke.args4j.Option

  import scala.collection.breakOut

  //basic options
  @Option(name="-in", aliases=Array("--input"),
    usage="input jar file to process. Exactly one of -in or -inPath must be specified"  )
  private var inputFileV:File = _
  def inputFile = inputFileV

  @Option(name="-inPath", aliases=Array("--inputPath"),
    usage="input jar files to process seperated by path seperator. Exactly one of -in or -inPath must be specified"  )
  private var inputPathV:String = _
  def inputPath = inputPathV

  @Option(name="-out", aliases=Array("--output"),
    usage="output jar file to process - default is to use the same name as the input filename, but in the local directory. " +
      "If the input is a path, then the output should be a directory, and the files will have names assigned based on the input files names"  )
  private var outputFileV:File = _
  lazy val outputFile = scala.Option(outputFileV).getOrElse(new File(inputFile.getName))

  @Option(name="-e", aliases=Array("--echoPath"),
    usage="if specified then the output jar[s] full qualified names are written seperated with the path seperator"  )
  private var echoPathV:File = _
  lazy val echoPath = echoPathV

  @Option(name="-over",aliases=Array("--overwriteOutput"),
    usage="overwrite the output file if it exists"  )
  private var overwriteV:Boolean = _
  def overwrite = overwriteV

  //what to strip
  @Option(name="-sd", aliases=Array("--stripDeprecated"),
    usage=" strip deprecated classes, methods and fields"  )
  private var stripDeprecatedV:Boolean = _
  def stripDeprecated = stripDeprecatedV

  @Option(name="-si", aliases=Array("--stripInner"),
    usage=" strip inner classes that are not referenced in the outer class"  )
  private var stripInnerV:Boolean = _
  def stripInner = stripInnerV

  @Option(name="-ss", aliases=Array("--stripScala"),
    usage="reduce to the minimum, classes compiled withthe scalac compiler, making use of the scala special attributes"  )
  private var stripScalaV:Boolean = _
  def stripScala = stripScalaV

  @Option(name="-sp", aliases=Array("--stripPackageRegex"), handler = classOf[StringArrayOptionHandler],
    usage=""" strip class/field/method which are java package protected, where th class matches these regex. This can take a list of regex values, e.g. -sp "sun\\..*  """)
  private var stripPackageRegexV:Array[String] = Array()
  lazy val stripPackageRegex:List[Regex] = stripPackageRegexV.map (_.r)(breakOut)

  @Option(name="-sr", aliases=Array("--stripRegex"), handler = classOf[StringArrayOptionHandler],
    usage=" strip classes which match regex. This can take a list of regex values"  )
  private var stripRegexV:Array[String] = Array()
  lazy val stripRegex:List[Regex] = stripRegexV.map (_.r)(breakOut)

  //admin
  @Option(name="-h", aliases=Array("-?", "--help", "-help"),
    usage="print help"  )
  private var helpV:Boolean = _
  def help = helpV

  @Option(name="-v", aliases=Array("--verbose"),
    usage="be verbose"  )
  private var verboseV:Boolean = _
  def verbose = verboseV

  //debugging

  @Option(name="-jp", aliases=Array("--javap"),
    usage="run javap on some of the result"  )
  private var javapV:String = _
  def javaP = javapV




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
    if ((inputFile eq null) && (inputPath eq null)) {
      parametersInError("Exactly one of -in or -inPath must be specified")
    }
  }
}
