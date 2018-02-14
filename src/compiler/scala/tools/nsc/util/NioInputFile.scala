package scala.tools.nsc.util

import java.io.{BufferedOutputStream, File, InputStream, OutputStream}
import java.net.URL
import java.nio.channels.Channels
import java.nio.file.{Files, LinkOption, Path, Paths}
import java.nio.file.attribute.BasicFileAttributes

import scala.reflect.io.{AbstractFile, PlainNioFile}

private[nsc] object NioInputFile {
  private val NoOptions = Array.empty[LinkOption]
  private val NoStrings = Array.empty[String]

  def apply (nioPath: Path): NioInputFile = this (nioPath,
    Files.readAttributes(nioPath, classOf[BasicFileAttributes], NioInputFile.NoOptions: _*))
  def apply  (path: String): NioInputFile = this (Paths.get(path, NioInputFile.NoStrings: _*))
  def apply (nioPath: Path, attributes: BasicFileAttributes): NioInputFile = new NioInputFile(nioPath, attributes)
}

private[nsc] class NioInputFile private (
    nioPath: Path,
    attributes: BasicFileAttributes) extends PlainNioFile(nioPath) {

  assert(attributes.isRegularFile)

  override def sizeOption = Some(attributes.size.toInt)

  /** Is this abstract file a directory? */
  override def isDirectory: Boolean = false

  /** Returns the time that this abstract file was last modified. */
  override def lastModified: Long = attributes.lastModifiedTime().toMillis

  override def iterator: Iterator[AbstractFile] = Iterator.empty

  override def file: File = super.file

  override lazy val canonicalPath: String = ??? //super.canonicalPath

  override def underlyingSource: Some[PlainNioFile] = super.underlyingSource

  override def name: String = super.name

  override def path: String = super.path

  override def absolute: PlainNioFile = super.absolute

  override def container: AbstractFile = super.container

  override def input: InputStream = super.input

  override def output: OutputStream = unsupported("read only")

  override def hashCode(): Int = super.hashCode()

  override def equals(that: Any): Boolean = super.equals(that)

  override def lookupName(name: String, directory: Boolean): AbstractFile = super.lookupName(name, directory)

  override def create(): Unit = unsupported("read only")

  override def delete(): Unit = unsupported("read only")

  override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = super.lookupNameUnchecked(name, directory)

  override def hasExtension(other: String): Boolean = super.hasExtension(other)

  override def exists: Boolean = super.exists

  override def isClassContainer: Boolean = super.isClassContainer

  override def isVirtual: Boolean = super.isVirtual

  override def bufferedOutput: BufferedOutputStream = unsupported("read only")

  override def toURL: URL = super.toURL

  override def toCharArray: Array[Char] = super.toCharArray

  override def toByteArray: Array[Byte] = super.toByteArray
  private var byteArray: Array[Byte] = _
  def readByteArray(): Unit = {
    val res = new Array[Byte](attributes.size().toInt)
    Files.lines()
  }

  override def lookupPathUnchecked(path: String, directory: Boolean): AbstractFile = super.lookupPathUnchecked(path, directory)

  override def fileNamed(name: String): AbstractFile = super.fileNamed(name)

  override def subdirectoryNamed(name: String): AbstractFile = super.subdirectoryNamed(name)

  override def toString(): String = super.toString()
}
