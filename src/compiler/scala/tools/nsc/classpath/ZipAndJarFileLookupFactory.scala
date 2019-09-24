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

package scala.tools.nsc.classpath

import java.io.{Closeable, File}
import java.net.URL
import java.nio.file.Files
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import java.util.{Timer, TimerTask}
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.reflect.io.{AbstractFile, FileZipArchive, ManifestResources}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}
import scala.tools.nsc.{CloseableRegistry, Settings}
import FileUtils._
import scala.tools.nsc.classpath.ClassPathElement.ZipJarClassPathElement
import scala.tools.nsc.io.Jar
import scala.tools.util.PathResolverCaching

/**
 * A trait providing an optional cache for classpath entries obtained from zip and jar files.
 * It allows us to e.g. reduce significantly memory used by PresentationCompilers in Scala IDE
 * when there are a lot of projects having a lot of common dependencies.
 */
sealed trait ZipAndJarFileLookupFactory {
  private val cache = new FileBasedCache[ClassPath with Closeable]

  def create(zipFile: AbstractFile, settings: Settings, closeableRegistry: CloseableRegistry, pathResolverCaching: PathResolverCaching): ClassPath = {
    cache.checkCacheability(zipFile.toURL :: Nil, checkStamps = true, disableCache = settings.YdisableFlatCpCaching || zipFile.file == null) match {
      case Left(_) =>
        val result: ClassPath with Closeable = createForZipFile(zipFile.file, settings.releaseValue)
        closeableRegistry.registerClosable(result)
        result
      case Right(Seq(path)) =>
        cache.getOrCreate(List(path), () => createForZipFile(zipFile.file, settings.releaseValue), closeableRegistry, checkStamps = true)
    }
  }

  def create(zipFile: ZipJarClassPathElement, settings: Settings, closeableRegistry: CloseableRegistry, pathResolverCaching: PathResolverCaching): ClassPath = {
    if (pathResolverCaching.canCache(zipFile)) {
      cache.getOrCreate(List(zipFile.path), () => createForZipFile(zipFile.file, settings.releaseValue), closeableRegistry, checkStamps = true)
    } else {
      val result: ClassPath with Closeable = createForZipFile(zipFile.file, settings.releaseValue)
      closeableRegistry.registerClosable(result)
      result
    }
  }

  protected def createForManifest(manifest: ManifestResources): ClassPath with Closeable
  protected def createForZipFile(zipFile: File, release: Option[String]): ClassPath with Closeable
}

/**
 * Manages creation of classpath for class files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarClassPathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveClassPath(zipFile: File, override val release: Option[String])
    extends ZipArchiveFileLookup[ClassFileEntryImpl]
    with NoSourcePaths {

    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(PackageName(pkg), simpleClassName + ".class").map(_.file)
    }
    // This method is performance sensitive as it is used by SBT's ExtractDependencies phase.
    override def findClass(className: String): Option[ClassRepresentation] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(PackageName(pkg), simpleClassName + ".class")
    }

    override private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): ClassFileEntryImpl = ClassFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isClass
  }

  /**
   * This type of classpath is closely related to the support for JSR-223.
   * Its usage can be observed e.g. when running:
   * jrunscript -classpath scala-compiler.jar;scala-reflect.jar;scala-library.jar -l scala
   * with a particularly prepared scala-library.jar. It should have all classes listed in the manifest like e.g. this entry:
   * Name: scala/Function2$mcFJD$sp.class
   */
  private case class ManifestResourcesClassPath(file: ManifestResources) extends ClassPath with NoSourcePaths with Closeable {
    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(PackageName(pkg)).find(_.name == simpleClassName).map(_.file)
    }

    override def asClassPathStrings: Seq[String] = Seq(file.path)

    override def asURLs: Seq[URL] = file.toURLs()
    override def close(): Unit = file.close()

    import ManifestResourcesClassPath.PackageFileInfo
    import ManifestResourcesClassPath.PackageInfo

    /**
     * A cache mapping package name to abstract file for package directory and subpackages of given package.
     *
     * ManifestResources can iterate through the collections of entries from e.g. remote jar file.
     * We can't just specify the path to the concrete directory etc. so we can't just 'jump' into
     * given package, when it's needed. On the other hand we can iterate over entries to get
     * AbstractFiles, iterate over entries of these files etc.
     *
     * Instead of traversing a tree of AbstractFiles once and caching all entries or traversing each time,
     * when we need subpackages of a given package or its classes, we traverse once and cache only packages.
     * Classes for given package can be then easily loaded when they are needed.
     */
    private lazy val cachedPackages: collection.mutable.HashMap[String, PackageFileInfo] = {
      val packages = collection.mutable.HashMap[String, PackageFileInfo]()

      def getSubpackages(dir: AbstractFile): List[AbstractFile] =
        (for (file <- dir if file.isPackage) yield file)(collection.breakOut)

      @tailrec
      def traverse(packagePrefix: String,
                   filesForPrefix: List[AbstractFile],
                   subpackagesQueue: collection.mutable.Queue[PackageInfo]): Unit = filesForPrefix match {
        case pkgFile :: remainingFiles =>
          val subpackages = getSubpackages(pkgFile)
          val fullPkgName = packagePrefix + pkgFile.name
          packages.put(fullPkgName, PackageFileInfo(pkgFile, subpackages))
          val newPackagePrefix = fullPkgName + "."
          subpackagesQueue.enqueue(PackageInfo(newPackagePrefix, subpackages))
          traverse(packagePrefix, remainingFiles, subpackagesQueue)
        case Nil if subpackagesQueue.nonEmpty =>
          val PackageInfo(packagePrefix, filesForPrefix) = subpackagesQueue.dequeue()
          traverse(packagePrefix, filesForPrefix, subpackagesQueue)
        case _ =>
      }

      val subpackages = getSubpackages(file)
      packages.put(ClassPath.RootPackage, PackageFileInfo(file, subpackages))
      traverse(ClassPath.RootPackage, subpackages, collection.mutable.Queue())
      packages
    }

    override private[nsc] def packages(inPackage: PackageName): Seq[PackageEntry] = cachedPackages.get(inPackage.dottedString) match {
      case None => Seq.empty
      case Some(PackageFileInfo(_, subpackages)) =>
        subpackages.map(packageFile => PackageEntryImpl(inPackage.entryName(packageFile.name)))
    }

    override private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = cachedPackages.get(inPackage.dottedString) match {
      case None => Seq.empty
      case Some(PackageFileInfo(pkg, _)) =>
        (for (file <- pkg if file.isClass) yield ClassFileEntryImpl(file))(collection.breakOut)
    }


    override private[nsc] def hasPackage(pkg: PackageName) = cachedPackages.contains(pkg.dottedString)
    override private[nsc] def list(inPackage: PackageName): ClassPathEntries = ClassPathEntries(packages(inPackage), classes(inPackage))
  }

  private object ManifestResourcesClassPath {
    case class PackageFileInfo(packageFile: AbstractFile, subpackages: Seq[AbstractFile])
    case class PackageInfo(packageName: String, subpackages: List[AbstractFile])
  }

  override protected def createForZipFile(zipFile: File, release: Option[String]): ClassPath with Closeable =
    ZipArchiveClassPath(zipFile, release)


  override protected def createForManifest(manifest: ManifestResources): ClassPath with Closeable =
      ManifestResourcesClassPath(manifest)
}

/**
 * Manages creation of classpath for source files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarSourcePathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveSourcePath(zipFile: File)
    extends ZipArchiveFileLookup[SourceFileEntryImpl]
    with NoClassPaths {
    def release: Option[String] = None

    override def asSourcePathString: String = asClassPathString

    override private[nsc] def sources(inPackage: PackageName): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntryImpl = SourceFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
  }

  override protected def createForZipFile(zipFile: File, release: Option[String]): ClassPath with Closeable = ZipArchiveSourcePath(zipFile)

  override protected def createForManifest(manifest: ManifestResources): ClassPath with Closeable = ???
}

final class FileBasedCache[T] {
  import java.nio.file.Path
  private case class Stamp(lastModified: FileTime, size: Long, fileKey: Object)
  private case class Entry(stamps: Seq[Stamp], t: T) {
    val referenceCount: AtomicInteger = new AtomicInteger(1)
    var timerTask: TimerTask = null
    def cancelTimer(): Unit = {
      timerTask match {
        case null =>
        case t => t.cancel()
      }
    }
  }
  private val cache = collection.mutable.Map.empty[Seq[Path], Entry]

  private def referenceCountDecrementer(e: Entry, paths: Seq[Path]): Closeable = {
    // Cancel the deferred close timer (if any) that was started when the reference count
    // last dropped to zero.
    e.cancelTimer()

    new Closeable {
      var closed = false
      override def close(): Unit = {
        if (!closed) {
          closed = true
          val count = e.referenceCount.decrementAndGet()
          if (count == 0) {
            e.t match {
              case cl: Closeable =>
                FileBasedCache.timer match {
                  case Some(timer) =>
                    val task = new TimerTask {
                      override def run(): Unit = {
                        cache.synchronized {
                          if (e.referenceCount.compareAndSet(0, -1)) {
                            cache.remove(paths)
                            cl.close()
                          }
                        }
                      }
                    }
                    e.timerTask = task
                    timer.schedule(task, FileBasedCache.deferCloseMs.toLong)
                  case None =>
                    cl.close()
                }
              case _ =>
            }
          }
        }
      }
    }
  }

  def checkCacheability(urls: Seq[URL], checkStamps: Boolean, disableCache: Boolean): Either[String, Seq[java.nio.file.Path]] = {
    import scala.reflect.io.{AbstractFile, Path}
    lazy val urlsAndFiles = urls.filterNot(_.getProtocol == "jrt").map(u => u -> AbstractFile.getURL(u))
    lazy val paths = urlsAndFiles.map(t => Path(t._2.file).jfile.toPath)
    if (disableCache) Left("caching is disabled due to a policy setting")
    else if (!checkStamps) Right(paths)
    else {
      val nonJarZips = urlsAndFiles.filter { case (url, file) => file == null || !Jar.isJarOrZip(file.file) }
      if (nonJarZips.nonEmpty) Left(s"caching is disabled because of the following classpath elements: ${nonJarZips.map(_._1).mkString(", ")}.")
      else Right(paths)
    }
  }
  def checkCacheability(url: URL, checkStamps: Boolean, disableCache: Boolean): Either[String, Option[java.nio.file.Path]] = {
    import scala.reflect.io.{AbstractFile, Path}
    if (disableCache) Left("caching is disabled due to a policy setting")
    else if (url.getProtocol == "jrt") Right(None)
    else {
      val file = AbstractFile.getURL(url).file
      if (!checkStamps) Right(Some(Path(file).jfile.toPath))
      else if ((file eq null) || !Jar.isJarOrZip(file.file))
        Left(s"caching is disabled because of the following classpath elements: $url.")
      else Right(Some(Path(file).jfile.toPath))
    }
  }
  def checkCacheability(file: AbstractFile, checkStamps: Boolean, disableCache: Boolean): Either[String, Option[java.nio.file.Path]] = {
    import scala.reflect.io.{Path}
    if (disableCache) Left("caching is disabled due to a policy setting")
    else if (!checkStamps) Right(Some(Path(file.file).jfile.toPath))
    else if ((file eq null) || !Jar.isJarOrZip(file.file))
      Left(s"caching is disabled because of the following classpath elements: $file.")
    else Right(Some(Path(file.file).jfile.toPath))
  }
  def checkCacheability2(file: AbstractFile, checkStamps: Boolean, disableCache: Boolean): Either[String, Option[java.nio.file.Path]] = {
    import scala.reflect.io.{Path}
    if (disableCache) Left("caching is disabled due to a policy setting")
    else if (!checkStamps) Right(Some(file.file.toPath))
    else if ((file eq null) || !Jar.isJarOrZip(file.file))
      Left(s"caching is disabled because of the following classpath elements: $file.")
    else Right(Some(file.file.toPath))
  }
  def checkCacheability3(file: AbstractFile, checkStamps: Boolean, disableCache: Boolean): Either[String, java.nio.file.Path] = {
    import scala.reflect.io.{Path}
    if (disableCache) Left("caching is disabled due to a policy setting")
    else if (!checkStamps) Right(file.file.toPath)
    else if ((file eq null) || !Jar.isJarOrZip(file.file))
      Left(s"caching is disabled because of the following classpath elements: $file.")
    else Right(file.file.toPath)
  }
  def checkCacheability4(file: AbstractFile, disableCache: Boolean): Either[String, java.nio.file.Path] = {
    if (disableCache) Left("caching is disabled due to a policy setting")
    else if ((file eq null) || !Jar.isJarOrZip(file.file))
      Left(s"caching is disabled because of the following classpath elements: $file.")
    else Right(file.file.toPath)
  }
  def checkCacheability(element: ZipJarClassPathElement, pathResolverCaching: PathResolverCaching): Either[String, java.nio.file.Path] = {
    if (pathResolverCaching.canCache(element)) Right(element.path) else Left("")
  }

  def getOrCreate(paths: Seq[Path], create: () => T, closeableRegistry: CloseableRegistry, checkStamps: Boolean): T = cache.synchronized {
    val stamps = if (!checkStamps) Nil else paths.map { path =>
      try {
      val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
      val lastModified = attrs.lastModifiedTime()
      // only null on some platforms, but that's okay, we just use the last modified timestamp as our stamp
      val fileKey = attrs.fileKey()
      Stamp(lastModified, attrs.size(), fileKey)
      } catch {
        case ex: java.nio.file.NoSuchFileException =>
          // Dummy stamp for (currently) non-existent file.
          Stamp(FileTime.fromMillis(0), -1, new Object)
      }
    }

    cache.get(paths) match {
      case Some(e@Entry(cachedStamps, cached)) =>
        if (!checkStamps || cachedStamps == stamps) {
          // Cache hit
          val count = e.referenceCount.incrementAndGet()
          assert(count > 0, (stamps, count))
          closeableRegistry.registerClosable(referenceCountDecrementer(e, paths))
          cached
        } else {
          // Cache miss: we found an entry but the underlying files have been modified
          cached match {
            case c: Closeable =>
              if (e.referenceCount.get() == 0) {
                c.close()
              } else {
                // TODO: What do do here? Maybe add to a list of closeables polled by a cleanup thread?
              }
          }
          val value = create()
          val entry = Entry(stamps, value)
          cache.put(paths, entry)
          closeableRegistry.registerClosable(referenceCountDecrementer(entry, paths))
          value
        }
      case _ =>
        // Cache miss
        val value = create()
        val entry = Entry(stamps, value)
        cache.put(paths, entry)
        closeableRegistry.registerClosable(referenceCountDecrementer(entry, paths))
        value
    }
  }

  def clear(): Unit = cache.synchronized {
    // TODO support closing
    // cache.valuesIterator.foreach(_.close())
    cache.clear()
  }
}
object FileBasedCache {
  // The tension here is that too long a delay could lead to an error (on Windows) with an inability
  // to overwrite the JAR. To short a delay and the entry could be evicted before a subsequent
  // sub-project compilation is able to get a cache hit. A more comprehensive solution would be to
  // involve build tools in the policy: they could close entries with refcount of zero when that
  // entry's JAR is about to be overwritten.
  private val deferCloseMs = Integer.getInteger("scalac.filebasedcache.defer.close.ms", 1000)
  private val timer: Option[Timer] = {
    if (deferCloseMs > 0)
      Some(new java.util.Timer(true))
    else None
  }
}
