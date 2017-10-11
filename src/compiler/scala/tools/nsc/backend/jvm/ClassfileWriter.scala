package scala.tools.nsc.backend.jvm

import java.io.{DataOutputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousFileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption, Path => NioPath}
import java.util
import java.util.concurrent.ConcurrentHashMap
import java.util.jar.Attributes.Name

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.reflect.internal.util.{NoPosition, SourceFile, Statistics}
import scala.reflect.io._
import scala.tools.nsc.Settings
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.{AbstractFile, Jar, JarWriter}
import scala.tools.nsc.transform.CleanUp

object ClassfileWriter {
  private def getDirectory(dir: String): AbstractFile =
    new PlainDirectory(Directory(Path(dir)))

  def apply(cleanup:CleanUp, settings:Settings, 
            statistics: Statistics with BackendStats,
            frontendAccess: PostProcessorFrontendAccess): ClassfileWriter = {
    import frontendAccess.{backendReporting}

    def singleWriter(file: AbstractFile): UnderlyingClassfileWriter = {
      if (file hasExtension "jar") {
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
        val jarMainAttrs = mainClass.map(c => Name.MAIN_CLASS -> c).toList
        val jar = new Jar(file.file).jarWriter(jarMainAttrs: _*)
        new JarClassWriter(jar)
      } else if (file.isVirtual) {
        new VirtualClassWriter()
      } else if (file.isDirectory) {
        new DirClassWriter()
      } else {
        ???
      }
    }
    val basicClassWriter = settings.outputDirs.getSingleOutput match {
      case Some(dir) => singleWriter(dir)
      case None =>
        val mappings: Map[AbstractFile, UnderlyingClassfileWriter] = settings.outputDirs.outputs.map {
          case (source, destination) => (source -> singleWriter(destination))
        }(scala.collection.breakOut)
        new MultiClassWriter(mappings)
    }
    val withStats = if (statistics.enabled) new WithStatsWriter(statistics, basicClassWriter) else basicClassWriter
    val withAdditionaFormats = if (settings.Ygenasmp.valueSetByUser.isEmpty && settings.Ydumpclasses.valueSetByUser.isEmpty) withStats else {
      val asmp = settings.Ygenasmp.valueSetByUser map getDirectory map { new AsmClassWriter(_)}
      val dump = settings.Ydumpclasses.valueSetByUser map getDirectory map { new DumpClassWriter(_)}
      new AllClassWriter(withStats, asmp, dump)
    }
    withAdditionaFormats
  }
  private final class JarClassWriter(jarWriter:scala.tools.nsc.io.JarWriter) extends UnderlyingClassfileWriter {

    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val path = className + ".class"
      val out = jarWriter.newOutputStream(path)
      try out.write(bytes, 0, bytes.length)
      finally out.flush()
    }
    override def close(): Unit = jarWriter.close()

    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = ()
  }
  private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    val index = clsName.lastIndexOf('/')
    val (packageName, simpleName) = if (index > 0) {
      (clsName.substring(0, index), clsName.substring(index + 1))
    } else ("", clsName)
    val directory = base.file.toPath.resolve(packageName)
    new PlainNioFile(directory.resolve(simpleName + suffix))
  }
  private def getPath(base: java.nio.file.Path, clsName: String, suffix: String): java.nio.file.Path = {
    base.resolve(clsName+suffix)
  }

  private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
    val outPath = outFile.file.toPath
    try Files.write(outPath, bytes)
    catch {
      case _: java.nio.file.NoSuchFileException =>
        Files.createDirectories(outPath.getParent)
        Files.write(outPath, bytes)
    }
  }


  private final class DirClassWriter() extends UnderlyingClassfileWriter {
    val openOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)

    val builtPaths = new ConcurrentHashMap[NioPath, java.lang.Boolean]()
    def ensureDirForPath(baseDir:NioPath, filePath: NioPath): Unit = {
      import java.lang.Boolean.TRUE
      val parent = filePath.getParent
      if (!builtPaths.containsKey(parent)) {
        Files.createDirectories(parent)
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

    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val path = unit.outputPath.resolve(className+".class")
      unit.addOperation

      ensureDirForPath(unit.outputPath, path)
      val os = AsynchronousFileChannel.open(path, openOptions, null)
      os.write(ByteBuffer.wrap(bytes), 0L, os, unit)
    }
    override def close(): Unit = ()
  }

  private final class AsmClassWriter(val asmOutputDir:AbstractFile) extends UnderlyingClassfileWriter {
    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val asmpFile = getFile(asmOutputDir, className, ".asmp")
      val asmpString = AsmUtils.textify(AsmUtils.readClass(bytes))
      writeBytes(asmpFile, asmpString.getBytes(StandardCharsets.UTF_8))
    }
    override def close(): Unit = ()

    //TODO - not critical though
    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = ()
  }
  private final class DumpClassWriter(val dumpOutputDir:AbstractFile) extends UnderlyingClassfileWriter {
    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val dumpFile = getFile(dumpOutputDir, className, ".class")
      writeBytes(dumpFile, bytes)
    }
    override def close(): Unit = ()
    //TODO - not critical though
    override def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit = ()
  }
  private final class VirtualClassWriter() extends UnderlyingClassfileWriter {

    private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
        def ensureDirectory(dir: AbstractFile): AbstractFile =
          if (dir.isDirectory) dir
          else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory", dir)

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

    def getUnderlying(unit: SourceUnit) = underlying.getOrElse(unit.outputDir, throw new IllegalStateException("TODO"))

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
  /** ensure the directory for unit exists
    * if there is any action to be performed, then it should occur in the specified evaluation context */
  def ensureDirectories(ec: ExecutionContextExecutor, unit: UnitResult): Unit

  def write(unit: SourceUnit, clazz: GeneratedClass, name: InternalName, bytes: Array[Byte])
  def close() : Unit
}
sealed trait UnderlyingClassfileWriter extends ClassfileWriter

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, val file: AbstractFile) extends IOException(msg)
