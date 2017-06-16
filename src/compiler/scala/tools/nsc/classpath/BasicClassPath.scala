package scala.tools.nsc.classpath

import java.io.{File, InputStream, OutputStream}
import java.net.URL
import java.nio.file.{Path, WatchEvent}
import java.util.zip.{ZipEntry, ZipFile}

import scala.collection.{Seq, mutable}
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.ClassPathWatcher.{BaseChangeListener, FileChangeListener}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}


abstract class FileClassPath extends ClassPath {

  val rootFile:File

  assert(rootFile != null, "file cannot be null")

  override final def asURLs: Seq[URL] = Seq(rootFile.toURI.toURL)
  override final def asClassPathStrings: Seq[String] = Seq(rootFile.getPath)

}
abstract class BasicClassPath extends FileClassPath  {
  override private[nsc] final def packages(inPackage: String) = list(inPackage).packages
}


abstract class UsageTrackedFileClassPath extends BasicClassPath {

  val fileChangeListener : BaseChangeListener

  private var inUseCount = 0
  protected def inUse(inUseNow:Boolean, executionContext: ExecutionContext) :Unit

  override def startInUse(executionContext: ExecutionContext, proactive:Boolean): Unit = if (!immutable || (inUseCount == 0)) fileChangeListener.synchronized{
    inUseCount += 1
    if (inUseCount == 1) {
      fileChangeListener.resumeIfSuspended()
      inUse(true, executionContext)
    }
  }

  override def endInUse(executionContext: ExecutionContext): Unit = if (!immutable) fileChangeListener.synchronized{
    inUseCount -= 1
    if (inUseCount == 0) inUse(false, executionContext)
  }

  override def makeCacheValid(executionContext: ExecutionContext, proactive:Boolean): Long =
    if (immutable) 0L
    else fileChangeListener.lastChangeNs

}

abstract class CommonClassPath[FileEntryType <: SingleClassRepresentation] extends UsageTrackedFileClassPath {
  self: TypedClassPath[FileEntryType] =>

  protected type ContentType <: ClassPathContent[FileEntryType]
  protected var content : ContentType = _

  /** Allows to get entries for packages and classes merged with sources possibly in one pass. */
  override private[nsc] def list(inPackage: String) =  content.data(inPackage).list
  private[nsc] def files(inPackage: String) : Seq[FileEntryType] = self.content.data(inPackage).files

  override protected def inUse(inUseNow: Boolean, executionContext: ExecutionContext): Unit = fileChangeListener.synchronized {
    if (inUseNow) {
      if (content eq null) {
        content = newContent()
        content.reOpen()
        content.startScan(executionContext)
      }
    }
    else {
      if (content ne null) content.close()
    }
  }
  protected def newContent() : ContentType

}
abstract class CommonZipClassPath[FileEntryType <: SingleClassRepresentation]( override val rootFile: File) extends
  CommonClassPath[FileEntryType] {
  self: TypedClassPath[FileEntryType] =>

  type ContentType = ZipArchiveContent[FileEntryType]

  override object fileChangeListener extends FileChangeListener(rootFile.toPath) {
    override protected def fileChanged(events: List[WatchEvent[Path]]) = {
      content = null
      true
    }
  }

  override protected def newContent(): ZipArchiveContent[FileEntryType] = new ZipArchiveContent(rootFile, isValidFilename, toRepr)
}

class RawZipClassesPath(rootFile :File) extends CommonZipClassPath[ClassFileEntry](rootFile) with CommonNoSourcesClassPath {

}
class RawZipSourcesPath(rootFile :File) extends CommonZipClassPath[SourceFileEntry](rootFile) with CommonNoClassesClassPath {

}
//
///**
//  * an implementation of Classpath with NIO and a file watcher.
//  * This enables the buiding of the caching to work efficiently
//  */
//trait NioDirectoryClassPath[FileEntryType <: ClassRepresentation] extends UsageTrackedFileClassPath {
//
//}
//
//class NioDirectoryClassesPath(val rootFile: File) extends CommonClassesPath{
//
//  override protected def inUse(inUseNow: Boolean, executionContext: ExecutionContext): Unit = ???
//
//}



/**
  * A trait allowing to look for classpath entries of given type in zip and jar files.
  * Based on raw java APIs
  * It provides common logic for classes handling class and source files.
  * It's aware of things META-INF directory which is correctly skipped from class loading.
  * The underlying jar or zip is lazyly opened when requested, and eagerly close thenthe classpath is not in use
  */
abstract class RawZipArchiveFileLookup[FileEntryType <: SingleClassRepresentation] extends UsageTrackedFileClassPath {

  // if null then the zip ile has changed
  //  content.zip is closed then the classpath is not is use, and lazyly opened when needed ( to read a file)
  private var content : ZipArchiveContent[FileEntryType] = _

  override object fileChangeListener extends FileChangeListener(rootFile.toPath) {
    override def fileChanged(changes: List[WatchEvent[Path]]): Boolean = fileChangeListener.synchronized {
      content = null
      true
    }
  }

  private def openContent(executionContext: ExecutionContext): Unit = {
    content = new ZipArchiveContent(rootFile, isValidFilename, toRepr)
    content.startScan(executionContext)
  }

  override protected def inUse(inUseNow: Boolean, executionContext: ExecutionContext): Unit = fileChangeListener.synchronized {
    if (inUseNow) {
      if (content eq null) {
        content = new ZipArchiveContent(rootFile, isValidFilename, toRepr)
        content.reOpen()
        content.startScan(executionContext)
      }
    }
    else {
      if (content ne null) content.close()
    }
  }
  protected def isValidFilename(fileName:String):Boolean
  protected def toRepr(abstractFile: AbstractFile):FileEntryType

  protected def files(inPackage: String): Seq[FileEntryType] =
    content.data(inPackage).files

  protected def filesByName(inPackage: String): Map[String, AbstractFile] =
    content.data(inPackage).filesByName

  override private[nsc] def list(inPackage: String): ClassPathEntries =
    content.data(inPackage).list

  //  private val javaZipFile = new ZipFile(zipFile)
  //  // if it exists in the achive load the linker data
  //  val linkerData = {
  //    val entry = javaZipFile.getEntry("META-INF/language/scala/linker.eager.ser")
  //    if (entry eq null) None
  //    else {
  //      println("Loading linker data for " + zipFile)
  //      Some(LinkerSymbol.readFrom(new DataInputStream(javaZipFile.getInputStream(entry))))
  //    }
  //  }


}
abstract class ClassPathContent[FileEntryType <: SingleClassRepresentation](file: File,
                                                             isValid : String => Boolean,
                                                             toRepr : AbstractFile => FileEntryType ) {

  def startScan(executionContext: ExecutionContext): Unit = {
    Future(data)(executionContext)
  }
  final lazy val data :Map[String, PackageInfo] = buildData
  protected def buildData() :Map[String, PackageInfo]

  class PackageInfo(packageName:String, val list:ClassPathEntries, val files:Seq[FileEntryType], val packages: Seq[PackageEntryImpl]) {
    lazy val filesByName:Map[String,AbstractFile] = files.map {
      file => file.name -> file.file}(collection.breakOut)
  }
  def reOpen() : Unit
  def close() :Unit

  }
class ZipArchiveContent[FileEntryType <: SingleClassRepresentation](zipFile: File,
                                                              isValid : String => Boolean,
                                                              toRepr : AbstractFile => FileEntryType )
extends ClassPathContent(zipFile, isValid, toRepr) {
  override def reOpen() = {
    zip = Some(new ZipFile(zipFile))
    println(s"open ${zipFile}")
  }

  private var zip :Option[ZipFile] = Some(new ZipFile(zipFile))
  override def close() = this.synchronized {
    zip.foreach{ z =>
      z.close()
      println(s"close ${zipFile}")
    }
    zip = None
  }


  protected def buildData(): Map[String, PackageInfo] = {
    //TODO use ser format
    class MutablePackageInfo(packageName:String) {
      val packages = Vector.newBuilder[PackageEntryImpl]
      val files = Vector.newBuilder[FileEntryType]
      def result(): PackageInfo = {
        val packagesResult = packages.result()
        val filesResult = files.result()
        val entries = new ClassPathEntries(packagesResult, filesResult)
        new PackageInfo(packageName, entries, filesResult, packagesResult)
      }
    }
    val staging = mutable.Map[String,MutablePackageInfo]()
    def getPackageInfo(packageName:String): MutablePackageInfo = {
      staging.get(packageName) match {
        case Some(res) => res
        case None =>
          val res = new MutablePackageInfo(packageName)
          staging.update(packageName, res)
          if (packageName != "") {
            val parentPackageName = packageName.take(packageName.lastIndexOf('.'))
            getPackageInfo(parentPackageName).packages += PackageEntryImpl(packageName)
          }
          res
      }
    }

    this.synchronized {
      val toClose = if (zip isEmpty) Some(new ZipFile(zipFile)) else None
      val myZip = zip.orElse(toClose).get
      try {
        val entries = myZip.entries()
        while (entries.hasMoreElements) {
          val entry = entries.nextElement()
          val name = entry.getName
          if (isValid(name)) {
            val dirSeparator = name.lastIndexOf('/')
            val packageName = name.take(dirSeparator).replace('/', '.')
            val fileName = name.substring(dirSeparator + 1, name.lastIndexOf('.'))
            getPackageInfo(packageName).files += toRepr(new ZipEntryFile(entry))
          }
        }
      } finally toClose foreach (_.close())
    }
    val res : Map[String, PackageInfo] = staging.map {
      case (k,v) => ( k-> v.result())
    } (collection.breakOut)
    res.withDefaultValue(new PackageInfo("", ClassPathEntries.empty, Nil, Nil))
  }
  private class ZipEntryFile(private val entry:ZipEntry) extends AbstractFile{

    /** Returns the name of this abstract file. */
    override val name: String = entry.getName.substring(entry.getName.lastIndexOf('/')+1)

    /** returns an input stream so the file can be read */
    override def input: InputStream = try {
      ZipArchiveContent.this.zip.get.getInputStream(entry)
    } catch {
      case t: Throwable =>
        println(s"failed to read ${ZipArchiveContent.this.zipFile}")
        t.printStackTrace()
        throw t
    }

    override def lastModified: Long = entry.getLastModifiedTime.toMillis

    //the rest is minimal support from AbstractFile API
    //used by toString()
    override def path: String = entry.getName

    override def absolute: AbstractFile = notImplemented

    override def container: AbstractFile = notImplemented

    override def file: File = notImplemented

    override def create(): Unit = notImplemented

    override def delete(): Unit = notImplemented

    override def isDirectory: Boolean = notImplemented

    override def output: OutputStream = notImplemented

    override def iterator: Iterator[AbstractFile] = notImplemented

    override def lookupName(name: String, directory: Boolean): AbstractFile = notImplemented

    override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = notImplemented

    private def notImplemented = ???

  }

}
