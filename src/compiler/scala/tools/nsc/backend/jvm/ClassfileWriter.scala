package scala.tools.nsc.backend.jvm

import java.io.{DataOutputStream, IOException}
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.util
import java.util.jar.Attributes.Name

import scala.reflect.internal.util.{NoPosition, Statistics}
import scala.reflect.io.PlainNioFile
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.io.AbstractFile

object ClassfileWriter {

  import scala.reflect.io.AbstractFile

  private def getDirectory(dir: Option[String]): Option[Path] =
    dir map { d => Paths.get(d) }

  def apply(frontendAccess: PostProcessorFrontendAccess): ClassfileWriter = {
    import frontendAccess.{compilerSettings, backendReporting}

    val basicOutputs = compilerSettings.singleOutputDirectory map {
      basicWriter(frontendAccess, _)
    } getOrElse {
      val mappings: Map[AbstractFile, ClassfileWriter] = compilerSettings.multiOutputDirectory.map {
        case ((src, target)) => src -> basicWriter(frontendAccess, target)
      }(scala.collection.breakOut)
      new MultiTargetClassfileWriter(frontendAccess, mappings)
    }
    val extras = {
      val builder = List.newBuilder[ClassfileWriter]

      getDirectory(compilerSettings.genAsmpDirectory) foreach {
        builder += new AsmpClassfileWriter(frontendAccess, _)
      }
      getDirectory(compilerSettings.dumpClassesDirectory) foreach {
        builder += new DumpClassfileWriter(frontendAccess, _)
      }
      builder.result()
    }
    if (extras isEmpty) basicOutputs else new ChainedClassfileWriter(basicOutputs, extras)
  }

  def basicWriter(frontendAccess: PostProcessorFrontendAccess, file: AbstractFile): ClassfileWriter = {
    import frontendAccess.{compilerSettings, backendReporting}
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
      val jarfile = new scala.tools.nsc.io.Jar(file.file).jarWriter(jarMainAttrs: _*)
      new DefaultJarClassfileWriter(frontendAccess, jarfile)
    }
    else if (file.file eq null) new AbstractFileClassfileWriter(frontendAccess, file)
    else new DefaultFileClassfileWriter(frontendAccess, file.file.toPath)
  }

  private class DefaultFileClassfileWriter(access: PostProcessorFrontendAccess, base: Path) extends ClassfileWriter(access) {
    override def writeImpl(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
      val outFile = getPath(base, className, ".class")
      writeBytes(outFile, bytes)
    }
  }
  private class AbstractFileClassfileWriter(access: PostProcessorFrontendAccess, base: AbstractFile) extends ClassfileWriter(access) {

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

    override def writeImpl(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
      val outFile = getFile(base, className, ".class")
      writeBytes(outFile, bytes)
    }
  }
  //TODO convert to raw jar access
  private class DefaultJarClassfileWriter(access: PostProcessorFrontendAccess, jarWriter :scala.tools.nsc.io.JarWriter) extends ClassfileWriter(access) {

    override def writeImpl(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
      val path = className + ".class"
      val out = jarWriter.newOutputStream(path)
      try out.write(bytes, 0, bytes.length)
      finally out.flush()

    }
    override def close() = jarWriter.close()

  }
  private class AsmpClassfileWriter(access: PostProcessorFrontendAccess, asmOutputDir: Path) extends ClassfileWriter(access) {

    override def writeImpl(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
      val asmpFile = getPath(asmOutputDir, className, ".asmp")
      val asmpString = AsmUtils.textify(AsmUtils.readClass(bytes))
      writeBytes(asmpFile, asmpString.getBytes(StandardCharsets.UTF_8))
    }
  }
  private class DumpClassfileWriter(access: PostProcessorFrontendAccess, dumpOutputDir:Path) extends ClassfileWriter(access) {

    override def writeImpl(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
      val dumpFile = getPath(dumpOutputDir, className, ".class")
      writeBytes(dumpFile, bytes)
    }
  }
  private class ChainedClassfileWriter(initial : ClassfileWriter, additions: List[ClassfileWriter]) extends ClassfileWriter(initial.frontendAccess) {
    val all = initial :: additions

    override def writeImpl(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit =
      all foreach {_.write(className, bytes, sourceFile)}

    override def close(): Unit = all foreach (_.close())
  }
  private class MultiTargetClassfileWriter(access: PostProcessorFrontendAccess, destinations : Map[scala.tools.nsc.io.AbstractFile, ClassfileWriter]) extends ClassfileWriter(access) {
    import access.{backendReporting, compilerSettings}

    override def writeImpl(className: InternalName, bytes: Array[Byte], sourceFile: AbstractFile): Unit = {
      val outFolder = compilerSettings.outputDirectoryFor(sourceFile)
      destinations(outFolder).writeImpl(className,bytes, sourceFile)
    }

    override def close(): Unit = destinations.values foreach (_.close())
  }
}

sealed abstract class ClassfileWriter (val frontendAccess: PostProcessorFrontendAccess) {
  import frontendAccess.{backendReporting, compilerSettings}
  final def write(className: InternalName, bytes: Array[Byte], sourceFile: scala.reflect.io.AbstractFile): Unit =
    try writeImpl(className, bytes, sourceFile) catch {
      case e: FileConflictException =>
        backendReporting.error(NoPosition, s"error writing $className: ${e.getMessage}")
      case e: java.nio.file.FileSystemException =>
        if (compilerSettings.debug)
          e.printStackTrace()
        backendReporting.error(NoPosition, s"error writing $className: ${e.getClass.getName} ${e.getMessage}")
    }
  def writeImpl(className: InternalName, bytes: Array[Byte], sourceFile: scala.reflect.io.AbstractFile): Unit
  def close(): Unit = ()

  protected final def getPath(path: Path, clsName: String, suffix: String) : Path = {
    val index = clsName.lastIndexOf('/')
    val (packageName, simpleName) = if (index > 0) {
      (clsName.substring(0, index), clsName.substring(index + 1))
    } else ("", clsName)
    val directory = path.resolve(packageName)
    directory.resolve(simpleName + suffix)
  }
  val writeOptions = util.EnumSet.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
  protected final def writeBytes(outPath: Path, bytes: Array[Byte]): Unit = {
    try writeBytesImpl(outPath, bytes)
    catch {
      case _: java.nio.file.NoSuchFileException =>
        Files.createDirectories(outPath.getParent)
        writeBytesImpl(outPath, bytes)
    }
  }


  private def writeBytesImpl(outPath: Path, bytes: Array[Byte]) = {
    val stream = Channels.newOutputStream(outPath.getFileSystem.provider.newByteChannel(outPath, writeOptions))
    stream.write(bytes)
    stream.close()
  }
}

/** Can't output a file due to the state of the file system. */
class FileConflictException(msg: String, val file: AnyRef) extends IOException(msg)
