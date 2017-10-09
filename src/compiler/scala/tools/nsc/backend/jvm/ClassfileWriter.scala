package scala.tools.nsc.backend.jvm

import java.io.{DataOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.jar.Attributes.Name

import scala.reflect.internal.util.{NoPosition, Statistics}
import scala.reflect.io._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.{AbstractFile, Jar, JarWriter}

object ClassfileWriter {
  private def getDirectory(dir: String): AbstractFile =
    new PlainDirectory(Directory(Path(dir)))

  def apply(frontendAccess: PostProcessorFrontendAccess): ClassfileWriter = {
    import frontendAccess.{backendReporting, compilerSettings}

    def singleWriter(file: AbstractFile): UnderlyingClassfileWriter = {
      if (file hasExtension "jar") {
        // If no main class was specified, see if there's only one
        // entry point among the classes going into the jar.
        val mainClass = compilerSettings.mainClass match {
          case c@Some(m) =>
            backendReporting.log(s"Main-Class was specified: $m")
            c

          case None => frontendAccess.getEntryPoints match {
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
        new JarClassWriter(frontendAccess, jar)
      } else if (file.isVirtual) {
        new VirtualClassWriter(frontendAccess)
      } else if (file.isDirectory) {
        new DirClassWriter(frontendAccess)
      } else {
        ???
      }
    }
    val basicClassWriter = compilerSettings.singleOutputDirectory match {
      case Some(dir) => singleWriter(dir)
      case None =>
        val mappings: Map[AbstractFile, UnderlyingClassfileWriter] = compilerSettings.multiOutputDirectory.map {
          case (source, destination) => (source -> singleWriter(destination))
        }(scala.collection.breakOut)
        new MultiClassWriter(frontendAccess, mappings)
    }
    val withStats = if (Statistics.enabled) new WithStatsWriter(basicClassWriter) else basicClassWriter
    val withAdditionaFormats = if (compilerSettings.genAsmpDirectory.isEmpty && compilerSettings.dumpClassesDirectory.isEmpty) withStats else {
      val asmp = compilerSettings.genAsmpDirectory map getDirectory map { new AsmClassWriter(frontendAccess, _)}
      val dump = compilerSettings.dumpClassesDirectory map getDirectory map { new DumpClassWriter(frontendAccess, _)}
      new AllClassWriter(withStats, asmp, dump)
    }
    withAdditionaFormats
  }
  private final class JarClassWriter(val frontendAccess: PostProcessorFrontendAccess, jarWriter:scala.tools.nsc.io.JarWriter) extends UnderlyingClassfileWriter {

    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val path = className + ".class"
      val out = jarWriter.newOutputStream(path)
      try out.write(bytes, 0, bytes.length)
      finally out.flush()
    }
    override def close(): Unit = jarWriter.close()
  }
  private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
    val index = clsName.lastIndexOf('/')
    val (packageName, simpleName) = if (index > 0) {
      (clsName.substring(0, index), clsName.substring(index + 1))
    } else ("", clsName)
    val directory = base.file.toPath.resolve(packageName)
    new PlainNioFile(directory.resolve(simpleName + suffix))
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


  private final class DirClassWriter(val frontendAccess: PostProcessorFrontendAccess) extends UnderlyingClassfileWriter {
    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val outFile = getFile(unit.outputDir, className, ".class")
      writeBytes(outFile, bytes)
    }
    override def close(): Unit = ()
  }

  private final class AsmClassWriter(val frontendAccess: PostProcessorFrontendAccess, val asmOutputDir:AbstractFile) extends UnderlyingClassfileWriter {
    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val asmpFile = getFile(asmOutputDir, className, ".asmp")
      val asmpString = AsmUtils.textify(AsmUtils.readClass(bytes))
      writeBytes(asmpFile, asmpString.getBytes(StandardCharsets.UTF_8))
    }
    override def close(): Unit = ()
  }
  private final class DumpClassWriter(val frontendAccess: PostProcessorFrontendAccess, val dumpOutputDir:AbstractFile) extends UnderlyingClassfileWriter {
    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val dumpFile = getFile(dumpOutputDir, className, ".class")
      writeBytes(dumpFile, bytes)
    }
    override def close(): Unit = ()
  }
  private final class VirtualClassWriter(val frontendAccess: PostProcessorFrontendAccess) extends UnderlyingClassfileWriter {

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

    override def close(): Unit = ()
  }
  private final class MultiClassWriter(val frontendAccess: PostProcessorFrontendAccess, underlying: Map [AbstractFile, UnderlyingClassfileWriter]) extends ClassfileWriter {
    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      underlying.getOrElse(unit.outputDir, throw new IllegalStateException("TODO")).write(unit, clazz, className, bytes)
    }
    override def close(): Unit = underlying.values.foreach(_.close())

  }
  private final class AllClassWriter(basic: ClassfileWriter, asmp: Option[UnderlyingClassfileWriter], dump: Option[UnderlyingClassfileWriter]) extends ClassfileWriter {
    override val frontendAccess: PostProcessorFrontendAccess = basic.frontendAccess

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
  }
  private final class WithStatsWriter(underlying: ClassfileWriter) extends ClassfileWriter {
    override val frontendAccess: PostProcessorFrontendAccess = underlying.frontendAccess

    override def write(unit: SourceUnit, clazz: GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = {
      val snap = Statistics.startTimer(BackendStats.bcodeWriteTimer)
      underlying.write(unit, clazz, className, bytes)
      Statistics.stopTimer(BackendStats.bcodeWriteTimer, snap)
    }

    override def close(): Unit = underlying.close()
  }
}
sealed trait ClassfileWriter {
  val frontendAccess: PostProcessorFrontendAccess
  def write(unit: SourceUnit, clazz: GeneratedClass, name: InternalName, bytes: Array[Byte])
  def close() : Unit
}
sealed trait UnderlyingClassfileWriter extends ClassfileWriter {
}

//class ClassfileWriter(frontendAccess: PostProcessorFrontendAccess) {
//  import frontendAccess.{backendReporting, compilerSettings}
//
//  // if non-null, asm text files are written to this directory
//  private val asmOutputDir: AbstractFile = getDirectoryOrNull(compilerSettings.genAsmpDirectory)
//
//  // if non-null, classfiles are additionally written to this directory
//  private val dumpOutputDir: AbstractFile = getDirectoryOrNull(compilerSettings.dumpClassesDirectory)
//
//  // if non-null, classfiles are written to a jar instead of the output directory
//  private val jarWriter: JarWriter = compilerSettings.singleOutputDirectory match {
//    case Some(f) if f hasExtension "jar" =>
//      // If no main class was specified, see if there's only one
//      // entry point among the classes going into the jar.
//      val mainClass = compilerSettings.mainClass match {
//        case c @ Some(m) =>
//          backendReporting.log(s"Main-Class was specified: $m")
//          c
//
//        case None => frontendAccess.getEntryPoints match {
//          case Nil =>
//            backendReporting.log("No Main-Class designated or discovered.")
//            None
//          case name :: Nil =>
//            backendReporting.log(s"Unique entry point: setting Main-Class to $name")
//            Some(name)
//          case names =>
//            backendReporting.log(s"No Main-Class due to multiple entry points:\n  ${names.mkString("\n  ")}")
//            None
//        }
//      }
//      val jarMainAttrs = mainClass.map(c => Name.MAIN_CLASS -> c).toList
//      new Jar(f.file).jarWriter(jarMainAttrs: _*)
//
//    case _ => null
//  }
//
//  private def getDirectoryOrNull(dir: Option[String]): AbstractFile =
//    dir.map(d => new PlainDirectory(Directory(Path(d)))).orNull
//
//  private def getFile(base: AbstractFile, clsName: String, suffix: String): AbstractFile = {
//    if (base.file != null) {
//      fastGetFile(base, clsName, suffix)
//    } else {
//      def ensureDirectory(dir: AbstractFile): AbstractFile =
//        if (dir.isDirectory) dir
//        else throw new FileConflictException(s"${base.path}/$clsName$suffix: ${dir.path} is not a directory", dir)
//      var dir = base
//      val pathParts = clsName.split("[./]").toList
//      for (part <- pathParts.init) dir = ensureDirectory(dir) subdirectoryNamed part
//      ensureDirectory(dir) fileNamed pathParts.last + suffix
//    }
//  }
//
//  private def fastGetFile(base: AbstractFile, clsName: String, suffix: String) = {
//    val index = clsName.lastIndexOf('/')
//    val (packageName, simpleName) = if (index > 0) {
//      (clsName.substring(0, index), clsName.substring(index + 1))
//    } else ("", clsName)
//    val directory = base.file.toPath.resolve(packageName)
//    new PlainNioFile(directory.resolve(simpleName + suffix))
//  }
//
//  private def writeBytes(outFile: AbstractFile, bytes: Array[Byte]): Unit = {
//    if (outFile.file != null) {
//      val outPath = outFile.file.toPath
//      try Files.write(outPath, bytes)
//      catch {
//        case _: java.nio.file.NoSuchFileException =>
//          Files.createDirectories(outPath.getParent)
//          Files.write(outPath, bytes)
//      }
//    } else {
//      val out = new DataOutputStream(outFile.bufferedOutput)
//      try out.write(bytes, 0, bytes.length)
//      finally out.close()
//    }
//  }
//
//  def write(unitResult: SourceUnit, generatedClass:GeneratedClass, className: InternalName, bytes: Array[Byte]): Unit = try {
//    val writeStart = Statistics.startTimer(BackendStats.bcodeWriteTimer)
//    if (jarWriter == null) {
//      val outFolder = unitResult.outputDir
//      val outFile = getFile(outFolder, className, ".class")
//      writeBytes(outFile, bytes)
//    } else {
//      val path = className + ".class"
//      val out = jarWriter.newOutputStream(path)
//      try out.write(bytes, 0, bytes.length)
//      finally out.flush()
//    }
//    Statistics.stopTimer(BackendStats.bcodeWriteTimer, writeStart)
//
//    if (asmOutputDir != null) {
//      val asmpFile = getFile(asmOutputDir, className, ".asmp")
//      val asmpString = AsmUtils.textify(AsmUtils.readClass(bytes))
//      writeBytes(asmpFile, asmpString.getBytes(StandardCharsets.UTF_8))
//    }
//
//    if (dumpOutputDir != null) {
//      val dumpFile = getFile(dumpOutputDir, className, ".class")
//      writeBytes(dumpFile, bytes)
//    }
//  } catch {
//    case e: FileConflictException =>
//      backendReporting.error(NoPosition, s"error writing $className: ${e.getMessage}")
//    case e: java.nio.file.FileSystemException =>
//      if (compilerSettings.debug)
//        e.printStackTrace()
//      backendReporting.error(NoPosition, s"error writing $className: ${e.getClass.getName} ${e.getMessage}")
//  }
//
//  def close(): Unit = {
//    if (jarWriter != null) jarWriter.close()
//  }
//}

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, val file: AbstractFile) extends IOException(msg)
