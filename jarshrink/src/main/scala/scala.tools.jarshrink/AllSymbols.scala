package scala.tools.jarshrink

import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.internal.pickling.ByteCodecs
import scala.reflect.{ScalaLongSignature, ScalaSignature}

/**
  * Created by Mike Skells on 11/10/2016.
  */

trait ClassInfo {
  def javaClassName:String
  def internalClassName:String
  def outerJavaClassName:Option[String]
  lazy val packageName = {
    val i = internalClassName.lastIndexOf("/")
    if (i == -1) "" else internalClassName.substring(0, i)
  }

  def scalaSignature : Option[ScalaClassSignature]
  def entryName:String
}
object ScalaClassSignature {
  private val expectedSig = new Array[Byte](3)//5 0 0
  expectedSig(0) = 5

  def apply(scalaSignature: CapturedAnnotation, scalaSig: Array[Byte], inlineBytes: Option[Array[Byte]]): ScalaClassSignature = {
    val scalaSignatureBytes = {
      val value = scalaSignature.toMap
      require(value.size == 1)
      val asText = value.head match {
        case (ValBytes, text: String)  if scalaSignature.desc == ValScalaSignature => text
        case (ValBytes, text: Array[String]) if scalaSignature.desc == ValScalaLongSignature => text.mkString("")
        case _ => sys.error("unexpected data")
      }
      val bytes = asText.getBytes("UTF-16")
      val len = ByteCodecs.decode(bytes)
      java.util.Arrays.copyOf(bytes,len)
    }

    require (util.Arrays.equals(scalaSig,expectedSig), scalaSig.mkString(","))
    new ScalaClassSignature(scalaSignatureBytes, inlineBytes)

  }
  //IO
  def write(os: DataOutputStream, sig: ScalaClassSignature): Unit = {
    def writeBytes(bytes:Array[Byte]) {
      os.writeInt(bytes.length)
      os.write(bytes)
    }
    def writeOptionalBytes(bytes:Option[Array[Byte]]) {
      bytes match {
        case None => os.writeInt(-1)
        case Some(data) => writeBytes(data)
      }
    }
    writeBytes(sig.scalaSignatureBytes)
    writeOptionalBytes(sig.inline)
  }
  def read(is:DataInputStream):ScalaClassSignature = {
    def readBytes :Array[Byte] = {
      val res = new Array[Byte](is.readInt)
      is.readFully(res)
      res
    }
    def readOptionalBytes :Option[Array[Byte]] = {
      val length = is.readInt
      if (length == -1) None
      else {
        val res = new Array[Byte](length)
        is.readFully(res)
        Some(res)
      }
    }
    new ScalaClassSignature(readBytes, readOptionalBytes)
  }

  val ValScalaSignature = "L" + classOf[ScalaSignature].getName.replace('.', '/') + ";"
  val ValScalaLongSignature = "L" + classOf[ScalaLongSignature].getName.replace('.', '/') + ";"
  val ValScalaSig = "ScalaSig"
  val ValScala = "Scala"
  val ValScalaInlineInfo = "ScalaInlineInfo"
  val ValBytes = "bytes"

}
class ScalaClassSignature private (private val scalaSignatureBytes:Array[Byte], private val inline:Option[Array[Byte]])

object LazyWriterConstants {
  val TypeCollection:Byte = 0x80.toByte
  val TypeScalaEmpty:Byte = 0x81.toByte
  val TypeScalaChildren:Byte = 0x82.toByte
  val TypeJavaEmpty:Byte = 0x83.toByte
  val TypeJavaChildren:Byte = 0x84.toByte
}

object LinkerSymbol {
  import LazyWriterConstants._
  import scala.collection.mutable
  def readFrom(is:DataInputStream) = {
    val tpe = is.readByte()
    require (tpe == TypeCollection)
    readCollection(is)
  }
  private def readSymbol(is:DataInputStream):LinkerSymbol = {
    val tpe = is.readByte()
    tpe match {
      case TypeCollection => readCollection(is)
      case TypeScalaEmpty => readScala(is, false)
      case TypeScalaChildren => readScala(is, true)
      case TypeJavaEmpty => readJava(is, false)
      case TypeJavaChildren => readJava(is, true)
      case _ => throw new IllegalStateException(s"unexpected type $tpe")
    }
  }
  private def readScala(is:DataInputStream, hasChildren:Boolean):LinkerSymbol = {
    val name = is.readUTF()
    val content = readContent(hasChildren, is)
    val signature = ScalaClassSignature.read(is)
    new ScalaSymbol(name, signature, content)
  }
  private def readJava(is:DataInputStream, hasChildren:Boolean):LinkerSymbol = {
    val name = is.readUTF()
    val content = readContent(hasChildren, is)
    new JavaSymbol(name, content)
  }

  private def readCollection(is:DataInputStream):LinkerSymbol = {
    val content = readContent(true, is)
    new LinkerSymbolCollection(content)
  }
  private def readContent(nonEmpty:Boolean, is:DataInputStream) = {
    if (nonEmpty) {
      val remaining = is.readInt()
      val builder = new mutable.HashMap[String, LinkerSymbol]
      while (remaining > 0) {
        val name = is.readUTF()
        val value = readSymbol(is)
        builder.put(name, value)
      }
      builder.toMap
    } else Map.empty[String, LinkerSymbol]
  }
}
sealed trait LinkerSymbol
case class LinkerSymbolCollection(content : Map[String,LinkerSymbol]) extends LinkerSymbol
case class JavaSymbol(name:String, content : Map[String,LinkerSymbol]) extends LinkerSymbol
case class ScalaSymbol(name:String, scalaClassSignature:ScalaClassSignature, content : Map[String,LinkerSymbol]) extends LinkerSymbol


class LazySymbolsWriter{

  import scala.collection.mutable

  def findOrCreate(localName: String) =
    directChildren.getOrElseUpdate(localName, new LazySymbolsWriter)

  private val directChildren = mutable.HashMap[String, LazySymbolsWriter]()

  def addLocal(copier: ClassInfo, name:String): LazySymbolsWriter = {
    require (!directChildren.isDefinedAt(name))
    val newChild = copier.scalaSignature match {
      case None => new JavaClassReference(copier.entryName)
      case Some(sig) => new ScalaClassReference(copier.entryName, sig)
    }
    directChildren(name) = newChild
    newChild
  }

  def writeLazyStructureTo(os:DataOutputStream):Unit = {
    os.writeByte(LazyWriterConstants.TypeCollection)
    writeContent(os)
  }

  protected def writeContentImpl(os: DataOutputStream, content: mutable.HashMap[String, LazySymbolsWriter]): Unit = {
    os.writeInt(content.size)
    content foreach {
      case (name, value) =>
        os.writeUTF(name)
        value.writeLazyStructureTo(os)
    }
  }

  protected def writeContent(os: DataOutputStream): Unit = {
    writeContentImpl(os, directChildren)
  }

  protected def hasChildren = directChildren.isEmpty

}
final class JavaClassReference(val entryName:String) extends LazySymbolsWriter {
  override def writeLazyStructureTo(os:DataOutputStream):Unit = {
    os.writeByte(if (hasChildren) LazyWriterConstants.TypeJavaChildren else LazyWriterConstants.TypeJavaEmpty)
    os.writeUTF(entryName)
    if (hasChildren) writeContent(os)
  }

}
final class ScalaClassReference(val entryName:String, val scalaClassSignature: ScalaClassSignature) extends LazySymbolsWriter {
  override def writeLazyStructureTo(os:DataOutputStream):Unit = {
    os.writeByte(if (hasChildren) LazyWriterConstants.TypeScalaChildren else LazyWriterConstants.TypeScalaEmpty)
    os.writeUTF(entryName)
    ScalaClassSignature.write(os, scalaClassSignature)
    if (hasChildren) writeContent(os)
  }

}

class RootSymbolWriter extends LazySymbolsWriter {

  import scala.collection.mutable


  val allClasses = mutable.HashMap[String, LazySymbolsWriter]()
  val globalAllClasses = mutable.HashMap[String, LazySymbolsWriter]()

  def writeEagerStructureTo(os:DataOutputStream):Unit = {
    os.writeByte(LazyWriterConstants.TypeCollection)
    writeContentImpl(os, globalAllClasses)
  }



  def addClassRef(copier: ClassInfo) {

    copier.outerJavaClassName match {
      case None =>
        val path = copier.packageName.split("/")
        var current: LazySymbolsWriter = this

        for (p <- path) {
          current = current.findOrCreate(p)
        }

        // {"/foo/bar/Baz.class" => data}
        globalAllClasses(copier.javaClassName) = addLocal(copier, copier.javaClassName)

        allClasses(copier.javaClassName) = current.addLocal(copier, copier.javaClassName)
      case Some(outer) =>
        globalAllClasses(copier.javaClassName) = addLocal(copier, copier.javaClassName)
        allClasses(copier.javaClassName) = allClasses(outer).addLocal(copier, copier.javaClassName)
    }
  }
}