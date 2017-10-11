package scala.tools.nsc.backend.jvm

import java.io.{BufferedOutputStream, DataOutputStream, FileOutputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, FileChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files, Path, Paths, StandardOpenOption}
import java.util
import java.util.concurrent.{ConcurrentHashMap, ExecutorService}
import java.util.jar.Attributes.Name
import java.util.jar.JarOutputStream
import java.util.zip.{CRC32, Deflater, ZipEntry, ZipOutputStream}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.reflect.internal.util.{NoPosition, Statistics}
import scala.tools.nsc.Settings
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.{AbstractFile, Jar}
import scala.tools.nsc.profile.AsyncHelper
import scala.tools.nsc.transform.CleanUp
import scala.util.Try

object ClassfileWriter {
  private def getDirectory(dir: String): Path = Paths.get(dir)

  def apply(asyncHelper: AsyncHelper, cleanup:CleanUp, settings:Settings,
            statistics: Statistics with BackendStats,
            frontendAccess: PostProcessorFrontendAccess): ClassfileWriter = {
    import frontendAccess.backendReporting

    def singleWriter(file: AbstractFile): UnderlyingClassfileWriter = {
      if (file hasExtension "jar") {
        import java.util.jar._
        // If no main class was specified, see if there's only one
        // entry point among the classes going into the jar.
        val mainClass = settings.mainClass.valueSetByUser match {
          case c@Some(m) =>
            backendReporting.log(s"Main-Class was specified: $m")
            c

          case None => cleanup.getEntryPoints match {
            case Nil =>
              backendReporting.log("No Main-Class designated or discovered.")
              None
            case name :: Nil =>
              backendReporting.log(s"Unique entry point: setting Main-Class to $name")
              Some(name)
            case names =>
              backendReporting.log(s"No Main-Class due to multiple entry points:\n  ${names.mkString("\n  ")}")
              None
          }
        }
        val manifest = new Manifest()
        mainClass foreach {c => manifest.getMainAttributes.put(Name.MAIN_CLASS, c)}
        val jar = new JarOutputStream(new BufferedOutputStream(new FileOutputStream(file.file), 64000), manifest)
        val level = frontendAccess.compilerSettings.jarCompressionLevel
        jar.setLevel(level)
        val storeOnly = level == Deflater.NO_COMPRESSION
        if (storeOnly) jar.setMethod(ZipOutputStream.STORED)
        new JarClassWriter(storeOnly, jar)
      } else if (file.isVirtual) {
        new VirtualClassWriter()
      } else if (file.isDirectory) {
        new DirClassWriter(frontendAccess)
      } else {
        throw new IllegalStateException(s"don't know how to handle an output of $file [${file.getClass}]")
      }
    }
    val basicClassWriter = settings.outputDirs.getSingleOutput match {
      case Some(dest) => singleWriter(dest)
      case None =>
        val distinctOuputs:Set[AbstractFile] = settings.outputDirs.outputs.map (_._2)(scala.collection.breakOut)
        if (distinctOuputs.size == 1) singleWriter(distinctOuputs.head)
        else new MultiClassWriter(distinctOuputs.map{output:AbstractFile => output ->  singleWriter(output)}((scala.collection.breakOut)))
    }
    val withStats = if (statistics.enabled) new WithStatsWriter(statistics, basicClassWriter) else basicClassWriter
    val withAdditionalFormats = if (settings.Ygenasmp.valueSetByUser.isEmpty && settings.Ydumpclasses.valueSetByUser.isEmpty) withStats else {
      val asmp = settings.Ygenasmp.valueSetByUser map {dir:String => new AsmClassWriter(getDirectory(dir), frontendAccess)}
      val dump = settings.Ydumpclasses.valueSetByUser map {dir:String => new DumpClassWriter(getDirectory(dir), frontendAccess)}
      new AllClassWriter(withStats, asmp, dump)
    }
    withAdditionalFormats
  }
  private final class JarClassWriter(storeOnly:Boolean, jarWriter:JarOutputStream) extends UnderlyingClassfileWriter {

    lazy val crc = new CRC32
    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = this.synchronized{
      val path = className + ".class"
      val entry = new ZipEntry(path)
      if (storeOnly) {
        crc.reset()
        crc.update(bytes)
        entry.setCrc(crc.getValue)
      }
      jarWriter.putNextEntry(entry)
      try jarWriter.write(bytes, 0, bytes.length)
      finally jarWriter.flush()
    }
    override def close(): Unit = this.synchronized(jarWriter.close())

    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = ()
  }

  private sealed class DirClassWriter(frontendAccess: PostProcessorFrontendAccess) extends UnderlyingClassfileWriter {

    val sync:Boolean = frontendAccess.compilerSettings.syncFileIO
    val builtPaths = new ConcurrentHashMap[Path, java.lang.Boolean]()
    def ensureDirForPath(baseDir:Path, filePath: Path): Unit = {
      import java.lang.Boolean.TRUE
      val parent = filePath.getParent
      if (!builtPaths.containsKey(parent)) {
        try Files.createDirectories(parent)
        catch { case e: FileAlreadyExistsException =>
            throw new FileConflictException(s"Can't create directory $parent", e)
        }
        builtPaths.put(baseDir, TRUE)
        var current = parent
        while ((current ne null) && (null ne builtPaths.put(current, TRUE ))) {
          current = current.getParent
        }
      }
    }

    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = {
      val classes = unit.copyClasses
      Future{
        classes foreach { cls:GeneratedClass =>
          ensureDirForPath(unit.outputPath, unit.outputPath.resolve(cls.classNode.name+".class"))
        }
      }(ec)
    }

    protected def getPath(unit: SourceUnit, className: InternalName) =  unit.outputPath.resolve(className+".class")
    protected def formatData(rawBytes: Array[Byte]) = rawBytes
    protected def qualifier:String = ""

    // the common case is that we are are creating a new file, and on MS Windows the create and truncate is expensive
    // because there is not an options in the windows API that corresponds to this so the truncate is applied as a separate call
    // even if the file is new.
    // as this is rare, its best to always try to create a new file, and it that fails, then open with truncate if that fails

    val fastOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
    val fallbackOpenOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)

    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, rawBytes: Array[Byte]): Unit = try {
      val path = getPath(unit,className)
      val bytes = formatData(rawBytes)
      ensureDirForPath(unit.outputPath, path)
      if (sync) {
        val os = try FileChannel.open(path, fastOpenOptions)
        catch {
          case _: FileAlreadyExistsException => FileChannel.open(path, fallbackOpenOptions)
        }

        os.write(ByteBuffer.wrap(bytes), 0L)
        os.close()
      } else {
        unit.addOperation()

        val os = try AsynchronousFileChannel.open(path, fastOpenOptions, exec)
        catch {
          case _: FileAlreadyExistsException => AsynchronousFileChannel.open(path, fallbackOpenOptions, exec)
        }

        os.write(ByteBuffer.wrap(bytes), 0L, (os, className), unit)
      }
    } catch {
      case e: FileConflictException =>
        frontendAccess.backendReporting.error(NoPosition, s"error writing $className$qualifier: ${e.getMessage}")
      case e: java.nio.file.FileSystemException =>
        if (frontendAccess.compilerSettings.debug)
          e.printStackTrace()
        frontendAccess.backendReporting.error(NoPosition, s"error writing $className$qualifier: ${e.getClass.getName} ${e.getMessage}")

    }
    override def close(): Unit = ()
  }

  private final class AsmClassWriter(val asmOutputPath:Path, frontendAccess: PostProcessorFrontendAccess)  extends DirClassWriter(frontendAccess) {
    override protected def getPath(unit: SourceUnit, className: InternalName) =  asmOutputPath.resolve(className+".asmp")
    override protected def formatData(rawBytes: Array[Byte]) = AsmUtils.textify(AsmUtils.readClass(rawBytes)).getBytes(StandardCharsets.UTF_8)
    override protected def qualifier:String = " [for asmp]"
  }
  private final class DumpClassWriter(val dumpOutputPath:Path, frontendAccess: PostProcessorFrontendAccess) extends DirClassWriter(frontendAccess) {
    override protected def getPath(unit: SourceUnit, className: InternalName) =  dumpOutputPath.resolve(className+".class")
    override protected def qualifier:String = " [for dump]"
  }
  private final class VirtualClassWriter() extends UnderlyingClassfileWriter {

    private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
      def ensureDirectory(dir: AbstractFile): AbstractFile =
        if (dir.isDirectory) dir
        else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory")

      var dir = base
      val pathParts = clsName.split("[./]").toList
      for (part <- pathParts.init) dir = ensureDirectory(dir) subdirectoryNamed part
      ensureDirectory(dir) fileNamed pathParts.last + suffix
    }

    private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
      val out = new DataOutputStream(outFile.bufferedOutput)
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }


    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val outFile = getFile(unit.outputDir, className, ".class")
      writeBytes(outFile, bytes)
    }
    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = ()
    override def close(): Unit = ()
  }
  private final class MultiClassWriter(underlying: Map [AbstractFile, UnderlyingClassfileWriter]) extends ClassfileWriter {

    def getUnderlying(unit: SourceUnit) = underlying.getOrElse(unit.outputDir, {
      throw new Exception(s"Cannot determine output directory for ${unit.sourceFile} with output ${unit.outputDir}. Configured outputs are ${underlying.keySet}")
    })

    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      getUnderlying(unit).write(unit, clazz, className, bytes)
    }
    override def close(): Unit = underlying.values.foreach(_.close())

    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = {
      getUnderlying(unit).ensureDirectories(ec, unit)
    }
  }
  private final class AllClassWriter(basic: ClassfileWriter, asmp: Option[UnderlyingClassfileWriter], dump: Option[UnderlyingClassfileWriter]) extends ClassfileWriter {

    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      basic.write(unit, clazz, className, bytes)
      asmp.foreach(_.write(unit, clazz, className, bytes))
      dump.foreach(_.write(unit, clazz, className, bytes))
    }
    override def close(): Unit = {
      basic.close()
      asmp.foreach(_.close())
      dump.foreach(_.close())
    }

    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = {
      basic.ensureDirectories(ec, unit)
      asmp.foreach(_.ensureDirectories(ec, unit))
      dump.foreach(_.ensureDirectories(ec, unit))
    }
  }
  private final class WithStatsWriter(statistics: Statistics with BackendStats, underlying: ClassfileWriter) extends ClassfileWriter {

    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = underlying.ensureDirectories(ec,unit)

    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val snap = statistics.startTimer(statistics.bcodeWriteTimer)
      underlying.write(unit, clazz, className, bytes)
      statistics.stopTimer(statistics.bcodeWriteTimer, snap)
    }

    override def close(): Unit = underlying.close()
  }
}
sealed trait ClassfileWriter {
  /** try to create the directory for the outputs in the compilation unit
    * if that fails then we ignore the errors.
    * The main writer will create any directories that are needed and report more fully
    * any errors that occur
    * if there is any action to be performed, then it should occur in the specified evaluation context */
  def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit

  def write(unit: SourceUnit, clazz: GeneratedClass, name: InternalName, bytes: Array[Byte])
  def close() : Unit
  var exec: ExecutorService = null
}
sealed trait UnderlyingClassfileWriter extends ClassfileWriter
class AsyncClassfileWriter(val writingService: ExecutorService, threadCount: Int, underlying: ClassfileWriter) extends ClassfileWriter {
  private implicit val ec = ExecutionContext.fromExecutor(writingService)

  def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult) = underlying.ensureDirectories(ec, unit)
  override def write(unit: SourceUnit, clazz: GeneratedClass, name: InternalName, bytes: Array[Byte]): Unit = {
    unit.addOperation()
    val f = Future{
      unit.withBufferedReporter {
        underlying.write(unit, clazz, name, bytes)
      }
    }

    f.onComplete{result:Try[Unit] => unit.endOperation(result)}
  }
  override def close(): Unit = {
    writingService.shutdownNow()
    underlying.close()
  }

  override def toString = s"Async[$threadCount]($underlying)"
}

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, cause:Throwable = null) extends IOException(msg, cause)
