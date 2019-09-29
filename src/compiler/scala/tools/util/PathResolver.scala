/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools
package util

import java.net.URL
import java.util.concurrent.ConcurrentHashMap

import scala.tools.reflect.WrappedProperties.AccessControl
import scala.tools.nsc.{CloseableRegistry, Settings}
import scala.tools.nsc.util.ClassPath
import scala.reflect.io.{Directory, File, Path}
import PartialFunction.condOpt
import scala.tools.nsc.classpath._

// Loosely based on the draft specification at:
// https://wiki.scala-lang.org/display/SIW/Classpath

object PathResolver {
  // Imports property/environment functions which suppress security exceptions.
  import AccessControl._
  import scala.compat.Platform.EOL

  implicit class MkLines(val t: TraversableOnce[_]) extends AnyVal {
    def mkLines: String = t.mkString("", EOL, EOL)
    def mkLines(header: String, indented: Boolean = false, embraced: Boolean = false): String = {
      val space = "\u0020"
      val sep = if (indented) EOL + space * 2 else EOL
      val (lbrace, rbrace) = if (embraced) (space + "{", EOL + "}") else ("", "")
      t.mkString(header + lbrace + sep, sep, rbrace + EOL)
    }
  }
  implicit class AsLines(val s: String) extends AnyVal {
    // sm"""...""" could do this in one pass
    def asLines = s.trim.stripMargin.linesIterator.mkLines
  }

  /** pretty print class path */
  def ppcp(s: String) = ClassPath.split(s) match {
    case Nil      => ""
    case Seq(x)   => x
    case xs       => xs.mkString(EOL, EOL, "")
  }

  /** Values found solely by inspecting environment or property variables.
   */
  object Environment {
    import scala.collection.JavaConverters._

    private def searchForBootClasspath: String = {
      val props = System.getProperties
      // This formulation should be immune to ConcurrentModificationExceptions when system properties
      // we're unlucky enough to witness a partially published result of System.setProperty or direct
      // mutation of the System property map. stringPropertyNames internally uses the Enumeration interface,
      // rather than Iterator, and this disables the fail-fast ConcurrentModificationException.
      val propNames = props.stringPropertyNames()
      propNames.asScala collectFirst { case k if k endsWith ".boot.class.path" => props.getProperty(k) } getOrElse ""
    }

    /** Environment variables which java pays attention to so it
     *  seems we do as well.
     */
    def sourcePathEnv       = envOrElse("SOURCEPATH", "")

    def javaBootClassPath   = propOrElse("sun.boot.class.path", searchForBootClasspath)
    def javaExtDirs         = propOrEmpty("java.ext.dirs")
    def scalaHome           = propOrEmpty("scala.home")
    def scalaExtDirs        = propOrEmpty("scala.ext.dirs")

    /** The java classpath and whether to use it. */
    def javaUserClassPath   = propOrElse("java.class.path", "")
    def useJavaClassPath    = propOrFalse("scala.usejavacp")

    override def toString = s"""
      |object Environment {
      |  scalaHome          = $scalaHome (useJavaClassPath = $useJavaClassPath)
      |  javaBootClassPath  = <${javaBootClassPath.length} chars>
      |  javaExtDirs        = ${ppcp(javaExtDirs)}
      |  javaUserClassPath  = ${ppcp(javaUserClassPath)}
      |  scalaExtDirs       = ${ppcp(scalaExtDirs)}
      |}""".asLines
  }

  /** Default values based on those in Environment as interpreted according
   *  to the path resolution specification.
   */
  object Defaults {
    def scalaSourcePath   = Environment.sourcePathEnv
    def javaBootClassPath = Environment.javaBootClassPath
    def javaUserClassPath = Environment.javaUserClassPath
    def javaExtDirs       = Environment.javaExtDirs
    def useJavaClassPath  = Environment.useJavaClassPath

    def scalaHome         = Environment.scalaHome
    def scalaHomeDir      = Directory(scalaHome)
    def scalaLibDir       = Directory(scalaHomeDir / "lib")
    def scalaClassesDir   = Directory(scalaHomeDir / "classes")

    def scalaLibAsJar     = File(scalaLibDir / "scala-library.jar")
    def scalaLibAsDir     = Directory(scalaClassesDir / "library")

    def scalaLibDirFound: Option[Directory] =
      if (scalaLibAsJar.isFile) Some(scalaLibDir)
      else if (scalaLibAsDir.isDirectory) Some(scalaClassesDir)
      else None

    def scalaLibFound =
      if (scalaLibAsJar.isFile) scalaLibAsJar.path
      else if (scalaLibAsDir.isDirectory) scalaLibAsDir.path
      else ""

    // TODO It must be time for someone to figure out what all these things
    // are intended to do.  This is disabled here because it was causing all
    // the scala jars to end up on the classpath twice: one on the boot
    // classpath as set up by the runner (or regular classpath under -nobootcp)
    // and then again here.
    def scalaBootClassPath  = ""
    def scalaExtDirs = Environment.scalaExtDirs
    def scalaPluginPath = (scalaHomeDir / "misc" / "scala-devel" / "plugins").path

    override def toString = s"""
      |object Defaults {
      |  scalaHome            = $scalaHome
      |  javaBootClassPath    = ${ppcp(javaBootClassPath)}
      |  scalaLibDirFound     = $scalaLibDirFound
      |  scalaLibFound        = $scalaLibFound
      |  scalaBootClassPath   = ${ppcp(scalaBootClassPath)}
      |  scalaPluginPath      = ${ppcp(scalaPluginPath)}
      |}""".asLines
  }

  /** Locations discovered by supplemental heuristics.
   */
  object SupplementalLocations {

    /** The platform-specific support jar.
     *
     *  Usually this is `tools.jar` in the jdk/lib directory of the platform distribution.
     *
     *  The file location is determined by probing the lib directory under JDK_HOME or JAVA_HOME,
     *  if one of those environment variables is set, then the lib directory under java.home,
     *  and finally the lib directory under the parent of java.home. Or, as a last resort,
     *  search deeply under those locations (except for the parent of java.home, on the notion
     *  that if this is not a canonical installation, then that search would have little
     *  chance of succeeding).
     */
    def platformTools: Option[File] = {
      val jarName = "tools.jar"
      def jarPath(path: Path) = (path / "lib" / jarName).toFile
      def jarAt(path: Path) = {
        val f = jarPath(path)
        if (f.isFile) Some(f) else None
      }
      val jdkDir = {
        val d = Directory(jdkHome)
        if (d.isDirectory) Some(d) else None
      }
      def deeply(dir: Directory) = dir.deepFiles find (_.name == jarName)

      val home    = envOrSome("JDK_HOME", envOrNone("JAVA_HOME")) map (p => Path(p))
      val install = Some(Path(javaHome))

      (home flatMap jarAt) orElse (install flatMap jarAt) orElse (install map (_.parent) flatMap jarAt) orElse
        (jdkDir flatMap deeply)
    }
    override def toString = s"""
      |object SupplementalLocations {
      |  platformTools        = $platformTools
      |}""".asLines
  }

  /** With no arguments, show the interesting values in Environment and Defaults.
   *  If there are arguments, show those in Calculated as if those options had been
   *  given to a scala runner.
   */
  def main(args: Array[String]): Unit =
    if (args.isEmpty) {
      println(Environment)
      println(Defaults)
    } else {
      val settings = new Settings()
      val rest = settings.processArguments(args.toList, processAll = false)._2
      val registry = new CloseableRegistry
      try {
        val pr = new PathResolver(settings, registry)
        println("COMMAND: 'scala %s'".format(args.mkString(" ")))
        println("RESIDUAL: 'scala %s'\n".format(rest.mkString(" ")))

        pr.result match {
          case cp: AggregateClassPath =>
            println(s"ClassPath has ${cp.aggregates.size} entries and results in:\n${cp.asClassPathStrings}")
        }
      } finally {
        registry.close()
      }
    }

  def apply(settings: Settings, closeableRegistry: CloseableRegistry = new CloseableRegistry): PathResolver =
    new PathResolver(settings, closeableRegistry)
}

class PathResolver protected (settings: Settings, closeableRegistry: CloseableRegistry = new CloseableRegistry) {

  @deprecated("for bincompat in 2.12.x series", "2.12.9")  // TODO remove from 2.13.x
  def this(settings: Settings) = this(settings, new CloseableRegistry)

  protected val classPathFactory = new ClassPathFactory(settings, closeableRegistry)

  import PathResolver.{ AsLines, Defaults, ppcp }

  private def cmdLineOrElse(name: String, alt: String) = {
    (commandLineFor(name) match {
      case Some("") => None
      case x        => x
    }) getOrElse alt
  }

  private def commandLineFor(s: String): Option[String] = condOpt(s) {
    case "javabootclasspath"  => settings.javabootclasspath.value
    case "javaextdirs"        => settings.javaextdirs.value
    case "bootclasspath"      => settings.bootclasspath.value
    case "extdirs"            => settings.extdirs.value
    case "classpath" | "cp"   => settings.classpath.value
    case "sourcepath"         => settings.sourcepath.value
  }

  val calculated: Calculated = new DefaultCalculated
  abstract class Calculated {
    def containers: List[ClassPath]
    def basis: List[Traversable[ClassPath]]
  }
  class DefaultCalculated extends Calculated {
  /** Calculated values based on any given command line options, falling back on
   *  those in Defaults.
   */
    def scalaHome           = Defaults.scalaHome
    def useJavaClassPath    = settings.usejavacp.value || Defaults.useJavaClassPath
    def useManifestClassPath= settings.usemanifestcp.value
    def javaBootClassPath   = cmdLineOrElse("javabootclasspath", Defaults.javaBootClassPath)
    def javaExtDirs         = cmdLineOrElse("javaextdirs", Defaults.javaExtDirs)
    def javaUserClassPath   = if (useJavaClassPath) Defaults.javaUserClassPath else ""
    def scalaBootClassPath  = cmdLineOrElse("bootclasspath", Defaults.scalaBootClassPath)
    def scalaExtDirs        = cmdLineOrElse("extdirs", Defaults.scalaExtDirs)

    /** Scaladoc doesn't need any bootstrapping, otherwise will create errors such as:
     * [scaladoc] ../scala-trunk/src/reflect/scala/reflect/macros/Reifiers.scala:89: error: object api is not a member of package reflect
     * [scaladoc] case class ReificationException(val pos: reflect.api.PositionApi, val msg: String) extends Throwable(msg)
     * [scaladoc]                                              ^
     * because the bootstrapping will look at the sourcepath and create package "reflect" in "<root>"
     * and then when typing relative names, instead of picking <root>.scala.relect, typedIdentifier will pick up the
     * <root>.reflect package created by the bootstrapping. Thus, no bootstrapping for scaladoc!
     * TODO: we should refactor this as a separate -bootstrap option to have a clean implementation, no? */
    def sourcePath          = if (!settings.isScaladoc) cmdLineOrElse("sourcepath", Defaults.scalaSourcePath) else ""

    def userClassPath = settings.classpath.value  // default is specified by settings and can be overridden there


    // Assemble the elements!
    def basis = List[Traversable[ClassPath]](
      buildJrt(settings.releaseValue, classPathFactory, PathResolverNoCaching),              // 0. The Java 9+ classpath (backed by the ct.sym or jrt:/ virtual system, if available)
      buildJavaBootClassPath(javaBootClassPath, classPathFactory, PathResolverNoCaching),    // 1. The Java bootstrap class path.
      buildJavaExtClassPath(javaExtDirs, classPathFactory, PathResolverNoCaching),           // 2. The Java extension class path.
      buildJavaAppClassPath(javaUserClassPath, classPathFactory, PathResolverNoCaching),     // 3. The Java application class path.
      buildScalaBootClassPath(scalaBootClassPath, classPathFactory, PathResolverNoCaching),  // 4. The Scala boot class path.
      buildScalaExtClassPath(scalaExtDirs, classPathFactory, PathResolverNoCaching),         // 5. The Scala extension class path.
      buildScalaAppClassPath(userClassPath, classPathFactory, PathResolverNoCaching),        // 6. The Scala application class path.
      buildManifestClassPath(useManifestClassPath, classPathFactory, PathResolverNoCaching), // 8. The Manifest class path.
      buildSourceClassPath(sourcePath, classPathFactory, PathResolverNoCaching)              // 7. The Scala source path.
    )

    protected def buildJrt(release: Option[String],classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): Option[ClassPath] = JrtClassPath.apply(release, closeableRegistry)
    protected def buildJavaBootClassPath(path:String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = classPathFactory.classesInPath(path,pathResolverCaching)
    protected def buildJavaExtClassPath(path:String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = classPathFactory.contentsOfDirsInPath(path,pathResolverCaching)
    protected def buildJavaAppClassPath(path:String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = classPathFactory.classesInExpandedPath(path,pathResolverCaching)
    protected def buildScalaBootClassPath(path:String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = classPathFactory.classesInPath(path,pathResolverCaching)
    protected def buildScalaExtClassPath(path:String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = classPathFactory.contentsOfDirsInPath(path,pathResolverCaching)
    protected def buildScalaAppClassPath(path:String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = classPathFactory.classesInExpandedPath(path,pathResolverCaching)
    protected def buildManifestClassPath(use:Boolean,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = classPathFactory.classesInManifest(use,pathResolverCaching)
    protected def buildSourceClassPath(path:String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = classPathFactory.sourcesInPath(path,pathResolverCaching)

    lazy val containers = basis.flatten.distinct

    override def toString = s"""
      |object Calculated {
      |  scalaHome            = $scalaHome
      |  javaBootClassPath    = ${ppcp(javaBootClassPath)}
      |  javaExtDirs          = ${ppcp(javaExtDirs)}
      |  javaUserClassPath    = ${ppcp(javaUserClassPath)}
      |  useJavaClassPath     = $useJavaClassPath
      |  scalaBootClassPath   = ${ppcp(scalaBootClassPath)}
      |  scalaExtDirs         = ${ppcp(scalaExtDirs)}
      |  userClassPath        = ${ppcp(userClassPath)}
      |  sourcePath           = ${ppcp(sourcePath)}
      |}""".asLines
  }

  def containers = calculated.containers

  import PathResolver.MkLines

  def result: ClassPath = {
    val cp = computeResult()
    if (settings.Ylogcp) {
      Console print f"Classpath built from ${settings.toConciseString} %n"
      Console print s"Defaults: ${PathResolver.Defaults}"
      Console print s"Calculated: $calculated"

      val xs = (calculated.basis drop 2).flatten.distinct
      Console print (xs mkLines (s"After java boot/extdirs classpath has ${xs.size} entries:", indented = true))
    }
    cp
  }

  def resultAsURLs: Seq[URL] = result.asURLs

  @deprecated("Use resultAsURLs instead of this one", "2.11.5")
  def asURLs: List[URL] = resultAsURLs.toList

  protected def computeResult(): ClassPath = AggregateClassPath(containers.toIndexedSeq)
}
abstract class PathResolverCaching {
  def canCache(element: ClassPathElement.ZipJarClassPathElement): Boolean

}
object PathResolverNoCaching extends PathResolverCaching {
  def canCache(element: ClassPathElement.ZipJarClassPathElement): Boolean = false
}


class ConfigurablePathResolver(settings: Settings, closeableRegistry: CloseableRegistry)
  extends PathResolver(settings, closeableRegistry) {

}
object ReuseAllPathResolver {
  def create(settings: Settings, closeableRegistry: CloseableRegistry): PathResolver =
    new ReuseAllPathResolver(settings, new CloseableRegistry)

  private val jrtCache = new ConcurrentHashMap[Option[String], Option[ClassPath]]()
  private val javaRootCp = new ConcurrentHashMap[String, List[ClassPath]]()
  private val javaExtCp = new ConcurrentHashMap[String, List[ClassPath]]()
  private val javaAppCp = new ConcurrentHashMap[String, List[ClassPath]]()
  private val scalaRootCp = new ConcurrentHashMap[String, List[ClassPath]]()
  private val scalaExtCp = new ConcurrentHashMap[String, List[ClassPath]]()
  private val scalaAppCp = new ConcurrentHashMap[String, List[ClassPath]]()
  private val scalaManifestCp = new ConcurrentHashMap[Boolean, List[ClassPath]]()
  private val sourceCp = new ConcurrentHashMap[String, List[ClassPath]]()

  private def individualSourceCache = new ConcurrentHashMap[String, CachedClassPath]()
  private def individualClassCache = new ConcurrentHashMap[String, CachedClassPath]()
}
class ReuseAllPathResolver(settings: Settings, closeableRegistry: CloseableRegistry)
  extends PathResolver(settings, closeableRegistry) {
  import ReuseAllPathResolver._

  override val calculated: Calculated = new DefaultCalculated {

    def cached(cp: Option[ClassPath], name: String) = {
      cp.map(new CachedClassPath(_, name))
    }

    def cachedTogether(cp: List[ClassPath], name: String) = {
      cp match {
        case Nil => Nil
        case _ => List(new CachedClassPath(new AggregateClassPath(cp), name))
      }
    }
    def cachedIndividually(cp: List[ClassPath], individualCache: ConcurrentHashMap[String, CachedClassPath], name: String) = {
      cp match {
        case Nil => Nil
        case cp =>
          val cached = cp map { ele =>
            val key = ele.asClassPathString
            val newPath = new CachedClassPath(ele, name)
            val existingPath = individualCache.computeIfAbsent(key, k => newPath)
            if (existingPath eq null) newPath else existingPath
          }
          List(new CachedClassPath(new AggregateClassPath(cached), s"cached aggregate[$name]"))
      }
    }

    override protected def buildJrt(release: Option[String],classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): Option[ClassPath] = {
      jrtCache.computeIfAbsent(release, r => cached(super.buildJrt(r, classPathFactory, pathResolverCaching), "jrt"))
    }

    override protected def buildJavaBootClassPath(path: String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = {
      javaRootCp.computeIfAbsent(path, p => cachedTogether(super.buildJavaBootClassPath(p, classPathFactory, pathResolverCaching), "java boot cp"))
    }

    override protected def buildJavaExtClassPath(path: String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = {
      javaExtCp.computeIfAbsent(path, p => cachedTogether(super.buildJavaExtClassPath(p, classPathFactory, pathResolverCaching), "java ext cp"))
    }

    override protected def buildJavaAppClassPath(path: String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = {
      javaAppCp.computeIfAbsent(path, p => cachedTogether(super.buildJavaAppClassPath(p, classPathFactory, pathResolverCaching), "java app cp"))
    }

    override protected def buildScalaBootClassPath(path: String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = {
      scalaRootCp.computeIfAbsent(path, p => cachedTogether(super.buildScalaBootClassPath(p, classPathFactory, pathResolverCaching), "scala boot cp"))
    }

    override protected def buildScalaExtClassPath(path: String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = {
      scalaExtCp.computeIfAbsent(path, p => cachedTogether(super.buildScalaExtClassPath(p, classPathFactory, pathResolverCaching), "scala ext cp"))
    }

    override protected def buildScalaAppClassPath(path: String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = {
      scalaAppCp.computeIfAbsent(path, p => cachedIndividually(super.buildScalaAppClassPath(p, classPathFactory, pathResolverCaching), individualClassCache, "scala app cp"))
    }

    override protected def buildManifestClassPath(use: Boolean,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = {
      scalaManifestCp.computeIfAbsent(use, u => cachedTogether(super.buildManifestClassPath(u, classPathFactory, pathResolverCaching), "manifest cp"))
    }

    override protected def buildSourceClassPath(path: String,classPathFactory: ClassPathFactory, pathResolverCaching: PathResolverCaching): List[ClassPath] = {
      sourceCp.computeIfAbsent(path, p => cachedIndividually(super.buildSourceClassPath(p, classPathFactory, pathResolverCaching), individualSourceCache, "source cp"))
    }
  }
}
