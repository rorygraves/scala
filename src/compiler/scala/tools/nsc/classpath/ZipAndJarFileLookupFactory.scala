/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import java.net.URL
import java.util.Map

import scala.annotation.tailrec
import scala.reflect.io.{AbstractFile, FileZipArchive, ManifestResources}
import scala.tools.nsc.util.{CachedClassPath, ClassPath}
import scala.tools.nsc.Settings
import FileUtils._
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.tools.nsc.util.CachedClassPath.CachedClassPath

/**
 * A trait providing an optional cache for classpath entries obtained from zip and jar files.
 * It's possible to create such a cache assuming that entries in such files won't change (at
 * least will be the same each time we'll load classpath during the lifetime of JVM process)
 * - unlike class and source files in directories, which can be modified and recompiled.
 * It allows us to e.g. reduce significantly memory used by PresentationCompilers in Scala IDE
 * when there are a lot of projects having a lot of common dependencies.
 */
sealed trait ZipAndJarFileLookupFactory {

  def create(zipFile: AbstractFile, settings: Settings): ClassPath = {
    if (settings.YdisableFlatCpCaching) createForZipFile(zipFile)
    else ZipJarClasspathCache.createUsingCache(zipFile, settings, createForZipFile _)
  }

  protected def createForZipFile(zipFile: AbstractFile): ClassPath

}

/**
 * Manages creation of classpath for class files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarClassPathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveClassPath(zipFile: File)
    extends ZipArchiveFileLookup[ClassFileEntryImpl]
    with NoSourcePaths {

    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(pkg).find(_.name == simpleClassName).map(_.file)
    }

    override private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

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
  private case class ManifestResourcesClassPath(file: ManifestResources) extends ClassPath with NoSourcePaths {
    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(pkg).find(_.name == simpleClassName).map(_.file)
    }

    override def asClassPathStrings: Seq[String] = Seq(file.path)

    override def asURLs: Seq[URL] = file.toURLs()

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

    override private[nsc] def packages(inPackage: String): Seq[PackageEntry] = cachedPackages.get(inPackage) match {
      case None => Seq.empty
      case Some(PackageFileInfo(_, subpackages)) =>
        val prefix = PackageNameUtils.packagePrefix(inPackage)
        subpackages.map(packageFile => PackageEntryImpl(prefix + packageFile.name))
    }

    override private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = cachedPackages.get(inPackage) match {
      case None => Seq.empty
      case Some(PackageFileInfo(pkg, _)) =>
        (for (file <- pkg if file.isClass) yield ClassFileEntryImpl(file))(collection.breakOut)
    }

    override private[nsc] def list(inPackage: String): ClassPathEntries = ClassPathEntries(packages(inPackage), classes(inPackage))
  }

  private object ManifestResourcesClassPath {
    case class PackageFileInfo(packageFile: AbstractFile, subpackages: Seq[AbstractFile])
    case class PackageInfo(packageName: String, subpackages: List[AbstractFile])
  }

  override protected def createForZipFile(zipFile: AbstractFile): ClassPath =
    if (zipFile.file == null) createWithoutUnderlyingFile(zipFile)
    else ZipArchiveClassPath(zipFile.file)

  private def createWithoutUnderlyingFile(zipFile: AbstractFile) = zipFile match {
    case manifestRes: ManifestResources =>
      ManifestResourcesClassPath(manifestRes)
    case _ =>
      val errorMsg = s"Abstract files which don't have an underlying file and are not ManifestResources are not supported. There was $zipFile"
      throw new IllegalArgumentException(errorMsg)
  }
}

/**
 * Manages creation of classpath for source files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarSourcePathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveSourcePath(zipFile: File)
    extends ZipArchiveFileLookup[SourceFileEntryImpl]
    with NoClassPaths {

    override def asSourcePathString: String = asClassPathString

    override private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntryImpl = SourceFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
  }

  override protected def createForZipFile(zipFile: AbstractFile): ClassPath = ZipArchiveSourcePath(zipFile.file)
}

object ZipJarClasspathCache {
  import scala.collection.JavaConverters._
  def configureFromSettings(settings: Settings) :Unit = cache.synchronized {
    if (!settings.YClassPathCacheJars) {
      cacheSize = 0
      cache.clear()
    } else {
      cacheSize = settings.YClassPathCacheJarsSize.value
      if (cache.size() > cacheSize) {
        cache.keySet().iterator().asScala.drop(cacheSize) foreach(cache.remove)
      }
    }
  }

  private var cacheSize = 1
  private val cache = new java.util.LinkedHashMap[AbstractFile, (CachedFileInfo, ClassPath)] {
    override def removeEldestEntry(eldest: Map.Entry[AbstractFile, (CachedFileInfo, ClassPath)]): Boolean =
      size() > cacheSize
  }

  private [classpath] def createUsingCache(zipFile: AbstractFile, settings: Settings, createForZipFile : (AbstractFile) => ClassPath): ClassPath = cache.synchronized {
    def addNewClassPathInstance :ClassPath = {
      if (settings.verbose || settings.Ylogcp)
        println(s"$zipFile is not yet in the classpath cache")
      createForZipFile(zipFile)
    }
    def cached(classPath:ClassPath):ClassPath = {
      if (settings.verbose || settings.Ylogcp)
        println(s"$zipFile cached:${settings.YClassPathCacheJars.value}")
      if (settings.YClassPathCacheJars) CachedClassPath(classPath) else classPath
    }
    val newCacheInfo = CachedFileInfo(zipFile, settings)
    val newClassPath = cache.get(zipFile) match {
      case (oldCacheInfo, classPath)  =>
        if (oldCacheInfo == newCacheInfo) {
          if (settings.verbose || settings.Ylogcp)
            println(s"$zipFile from the classpath cache")
          classPath
        } else {
          if (settings.verbose || settings.Ylogcp)
            println(s"$zipFile is not up to date in the classpath cache")
          cached(createForZipFile(zipFile))
        }
      case miss =>
        if (settings.verbose || settings.Ylogcp)
          println(s"$zipFile is not yet in the classpath cache")
        cached(createForZipFile(zipFile))
    }
    cache.put(zipFile, (newCacheInfo, newClassPath))
    if (settings.YClassPathJarPrefetch) newClassPath match {
      case cached : CachedClassPath => cached.startPrefetch(ExecutionContext.Implicits.global)
      case _ =>
    }
    newClassPath
  }

  private def CachedFileInfo(zipFile: AbstractFile, settings: Settings) = {
    val file = zipFile.file
    new CachedFileInfo(file.getAbsolutePath, file.lastModified(), file.length(), settings.YClassPathCacheJars)
  }

  private case class CachedFileInfo(path: String, lastModified: Long, length: Long, cached:Boolean)

}
