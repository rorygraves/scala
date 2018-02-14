/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package io

import java.nio.channels.Channels
import java.nio.file.attribute.FileAttribute
import java.nio.file.spi.FileSystemProvider
import java.util

/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
class PlainDirectory(givenPath: Directory) extends PlainFile(givenPath) {
  override def isDirectory = true
  override def iterator = givenPath.list filter (_.exists) map (x => new PlainFile(x))
  override def delete(): Unit = givenPath.deleteRecursively()
}

/** This class implements an abstract file backed by a File.
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class PlainFile(val givenPath: Path) extends AbstractFile {
  assert(path ne null)

  val file = givenPath.jfile

  override lazy val canonicalPath = super.canonicalPath

  override def underlyingSource = Some(this)

  private val fpath = givenPath.toAbsolute

  /** Returns the name of this abstract file. */
  def name = givenPath.name

  /** Returns the path of this abstract file. */
  def path = givenPath.path

  /** The absolute file. */
  def absolute = new PlainFile(givenPath.toAbsolute)

  override def container: AbstractFile = new PlainFile(givenPath.parent)
  override def input = givenPath.toFile.inputStream()
  override def output = givenPath.toFile.outputStream()
  override def sizeOption = Some(givenPath.length.toInt)

  override def hashCode(): Int = fpath.hashCode()
  override def equals(that: Any): Boolean = that match {
    case x: PlainFile => fpath == x.fpath
    case _            => false
  }

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = givenPath.isDirectory

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = givenPath.lastModified

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    // Optimization: Assume that the file was not deleted and did not have permissions changed
    // between the call to `list` and the iteration. This saves a call to `exists`.
    def existsFast(path: Path) = path match {
      case (_: Directory | _: io.File) => true
      case _                           => path.exists
    }
    if (!isDirectory) Iterator.empty
    else givenPath.toDirectory.list filter existsFast map (new PlainFile(_))
  }

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    val child = givenPath / name
    if ((child.isDirectory && directory) || (child.isFile && !directory)) new PlainFile(child)
    else null
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = if (!exists) givenPath.createFile()

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit =
    if (givenPath.isFile) givenPath.delete()
    else if (givenPath.isDirectory) givenPath.toDirectory.deleteRecursively()

  /** Returns a plain file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainFile(givenPath / name)
}

private[scala] object PlainNioFile {
  val NoOpenOptions = util.EnumSet.noneOf(classOf[java.nio.file.OpenOption])
  val NoAttribues = Array.empty[FileAttribute[_]]

}
private[scala] class PlainNioFile(nioPath: java.nio.file.Path) extends AbstractFile {
  import java.nio.file._
  import PlainNioFile._

  assert(nioPath ne null)

  /** Returns the underlying File if any and null otherwise. */
  override def file: java.io.File = try {
    nioPath.toFile
  } catch {
    case _: UnsupportedOperationException => null
  }

  override lazy val canonicalPath = super.canonicalPath

  override def underlyingSource  = Some(this)

  private val fpath = nioPath.toAbsolutePath.toString

  /** Returns the name of this abstract file. */
  def name = nioPath.getFileName.toString

  /** Returns the path of this abstract file. */
  def path = nioPath.toString

  /** The absolute file. */
  def absolute = new PlainNioFile(nioPath.toAbsolutePath)

  override def container: AbstractFile = new PlainNioFile(nioPath.getParent)
  override def input = {
    provider(nioPath).;
    Channels.newInputStream(Files.newByteChannel(nioPath, NoOpenOptions, NoAttribues: _*))
    Files.newInputStream(nioPath, NoOpenOptions: _*)
  }
  override def output = Files.newOutputStream(nioPath, NoOpenOptions: _*)
  override def sizeOption = Some(Files.size(nioPath).toInt)
  override def hashCode(): Int = fpath.hashCode()
  override def equals(that: Any): Boolean = that match {
    case x: PlainNioFile => fpath == x.fpath
    case _               => false
  }

  private def provider(path: java.nio.file.Path) = path.getFileSystem.provider

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = Files.isDirectory(nioPath)

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = Files.getLastModifiedTime(nioPath).toMillis

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    try {
      import scala.collection.JavaConverters._
      val it = Files.newDirectoryStream(nioPath).iterator()
      it.asScala.map(new PlainNioFile(_))
    } catch {
      case _: NotDirectoryException => Iterator.empty
    }
  }

  /**
    * Returns the abstract file in this abstract directory with the
    * specified name. If there is no such file, returns null. The
    * argument "directory" tells whether to look for a directory or
    * or a regular file.
    */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    val child = nioPath.resolve(name)
    if ((Files.isDirectory(child) && directory) || (Files.isRegularFile(child) && !directory)) new PlainNioFile(child)
    else null
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = if (!exists)  Files.createFile(nioPath)

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit =
    if (Files.isRegularFile(nioPath)) Files.deleteIfExists(nioPath)
    else if (Files.isDirectory(nioPath)) new Directory(nioPath.toFile).deleteRecursively()

  /** Returns a plain file with the given name. It does not
    *  check that it exists.
    */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainNioFile(nioPath.resolve(name))
}
