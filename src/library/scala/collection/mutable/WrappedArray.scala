/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import scala.reflect.ClassTag
import scala.runtime.BoxedUnit
import scala.collection.generic._
import scala.collection.parallel.mutable.ParArray
import scala.util.hashing.MurmurHash3

import java.util.Arrays

/**
 *  A class representing `Array[T]`.
 *
 *  @tparam T    type of the elements in this wrapped array.
 *
 *  @author  Martin Odersky, Stephane Micheloud
 *  @version 1.0
 *  @since 2.8
 *  @define Coll `WrappedArray`
 *  @define coll wrapped array
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
abstract class WrappedArray[T]
extends AbstractSeq[T]
    with IndexedSeq[T]
    with ArrayLike[T, WrappedArray[T]]
    with CustomParallelizable[T, ParArray[T]]
{

  override protected[this] def thisCollection: WrappedArray[T] = this
  override protected[this] def toCollection(repr: WrappedArray[T]): WrappedArray[T] = repr

  /** The tag of the element type */
  def elemTag: ClassTag[T]

  @deprecated("use elemTag instead", "2.10.0")
  def elemManifest: ClassManifest[T] = ClassManifest.fromClass[T](elemTag.runtimeClass.asInstanceOf[Class[T]])

  /** The length of the array */
  def length: Int

  /** The element at given index */
  def apply(index: Int): T

  /** Update element at given index */
  def update(index: Int, elem: T): Unit

  /** The underlying array */
  def array: Array[T]

  override def par = ParArray.handoff(array)

  private def elementClass: Class[_] =
    array.getClass.getComponentType

  override def toArray[U >: T : ClassTag]: Array[U] = {
    val thatElementClass = implicitly[ClassTag[U]].runtimeClass
    if (elementClass eq thatElementClass)
      array.asInstanceOf[Array[U]]
    else
      super.toArray[U]
  }

  override def stringPrefix = "WrappedArray"

  /** Clones this object, including the underlying Array. */
  override def clone(): WrappedArray[T] = WrappedArray make array.clone()

  /** Creates new builder for this collection ==> move to subclasses
   */
  override protected[this] def newBuilder: Builder[T, WrappedArray[T]] =
    new WrappedArrayBuilder[T](elemTag)

}

/** A companion object used to create instances of `WrappedArray`.
 */
object WrappedArray {
  // This is reused for all calls to empty.
  private val EmptyWrappedArray  = new ofRef[AnyRef](new Array[AnyRef](0))
  def empty[T <: AnyRef]: WrappedArray[T] = EmptyWrappedArray.asInstanceOf[WrappedArray[T]]

  // If make is called explicitly we use whatever we're given, even if it's
  // empty.  This may be unnecessary (if WrappedArray is to honor the collections
  // contract all empty ones must be equal, so discriminating based on the reference
  // equality of an empty array should not come up) but we may as well be
  // conservative since wrapRefArray contributes most of the unnecessary allocations.
  def make[T](x: AnyRef): WrappedArray[T] = (x match {
    case null              => null
    case x: Array[AnyRef]  => new ofRef[AnyRef](x)
    case x: Array[Int]     => new ofInt(x)
    case x: Array[Double]  => new ofDouble(x)
    case x: Array[Long]    => new ofLong(x)
    case x: Array[Float]   => new ofFloat(x)
    case x: Array[Char]    => new ofChar(x)
    case x: Array[Byte]    => new ofByte(x)
    case x: Array[Short]   => new ofShort(x)
    case x: Array[Boolean] => new ofBoolean(x)
    case x: Array[Unit]    => new ofUnit(x)
  }).asInstanceOf[WrappedArray[T]]

  implicit def canBuildFrom[T](implicit m: ClassTag[T]): CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] =
    new CanBuildFrom[WrappedArray[_], T, WrappedArray[T]] {
      def apply(from: WrappedArray[_]): Builder[T, WrappedArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedArray.make[T]
      def apply: Builder[T, WrappedArray[T]] =
        ArrayBuilder.make[T]()(m) mapResult WrappedArray.make[T]
  }

  def newBuilder[A]: Builder[A, IndexedSeq[A]] = new ArrayBuffer

  import forCompiler._

  final class ofRef[T <: AnyRef](val array: Array[T]) extends WrappedArray[T] with Serializable {
    lazy val elemTag = ClassTag[T](array.getClass.getComponentType)
    def length: Int = array.length
    def apply(index: Int): T = array(index).asInstanceOf[T]
    def update(index: Int, elem: T) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofRef[_] => Arrays.equals(array.asInstanceOf[Array[AnyRef]], that.array.asInstanceOf[Array[AnyRef]])
      case that: ofRef1[_] if length == 1 => array(0).equals(that.value)
      case that: ofRef2[_] if length == 2 => array(0).equals(that.value1) && array(1).equals(that.value2)
      case _ => super.equals(that)
    }
  }

  final class ofByte(val array: Array[Byte]) extends WrappedArray[Byte] with Serializable {
    def elemTag = ClassTag.Byte
    def length: Int = array.length
    def apply(index: Int): Byte = array(index)
    def update(index: Int, elem: Byte) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedBytesHash(array)
    override def equals(that: Any) = that match {
      case that: ofByte => Arrays.equals(array, that.array)
      case that: ofByte1 if length == 1 => array(0) == that.value
      case that: ofByte2 if length == 2 => array(0) == that.value1 && array(1) == that.value2
      case _ => super.equals(that)
    }
  }

  final class ofShort(val array: Array[Short]) extends WrappedArray[Short] with Serializable {
    def elemTag = ClassTag.Short
    def length: Int = array.length
    def apply(index: Int): Short = array(index)
    def update(index: Int, elem: Short) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofShort => Arrays.equals(array, that.array)
      case that: ofShort1 if length == 1 => array(0) == that.value
      case that: ofShort2 if length == 2 => array(0) == that.value1 && array(1) == that.value2
      case _ => super.equals(that)
    }
  }

  final class ofChar(val array: Array[Char]) extends WrappedArray[Char] with Serializable {
    def elemTag = ClassTag.Char
    def length: Int = array.length
    def apply(index: Int): Char = array(index)
    def update(index: Int, elem: Char) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofChar => Arrays.equals(array, that.array)
      case that: ofChar1 if length == 1 => array(0) == that.value
      case that: ofChar2 if length == 2 => array(0) == that.value1 && array(1) == that.value2
      case _ => super.equals(that)
    }
  }

  final class ofInt(val array: Array[Int]) extends WrappedArray[Int] with Serializable {
    def elemTag = ClassTag.Int
    def length: Int = array.length
    def apply(index: Int): Int = array(index)
    def update(index: Int, elem: Int) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofInt => Arrays.equals(array, that.array)
      case that: ofInt1 if length == 1 => array(0) == that.value
      case that: ofInt2 if length == 2 => array(0) == that.value1 && array(1) == that.value2
      case _ => super.equals(that)
    }
  }

  final class ofLong(val array: Array[Long]) extends WrappedArray[Long] with Serializable {
    def elemTag = ClassTag.Long
    def length: Int = array.length
    def apply(index: Int): Long = array(index)
    def update(index: Int, elem: Long) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofLong => Arrays.equals(array, that.array)
      case that: ofLong1 if length == 1 => array(0) == that.value
      case that: ofLong2 if length == 2 => array(0) == that.value1 && array(1) == that.value2
      case _ => super.equals(that)
    }
  }

  final class ofFloat(val array: Array[Float]) extends WrappedArray[Float] with Serializable {
    def elemTag = ClassTag.Float
    def length: Int = array.length
    def apply(index: Int): Float = array(index)
    def update(index: Int, elem: Float) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofFloat => Arrays.equals(array, that.array)
      case that: ofFloat1 if length == 1 => array(0) == that.value
      case that: ofFloat2 if length == 2 => array(0) == that.value1 && array(1) == that.value2
      case _ => super.equals(that)
    }
  }

  final class ofDouble(val array: Array[Double]) extends WrappedArray[Double] with Serializable {
    def elemTag = ClassTag.Double
    def length: Int = array.length
    def apply(index: Int): Double = array(index)
    def update(index: Int, elem: Double) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofDouble => Arrays.equals(array, that.array)
      case that: ofDouble1 if length == 1 => array(0) == that.value
      case that: ofDouble2 if length == 2 => array(0) == that.value1 && array(1) == that.value2
      case _ => super.equals(that)
    }
  }

  final class ofBoolean(val array: Array[Boolean]) extends WrappedArray[Boolean] with Serializable {
    def elemTag = ClassTag.Boolean
    def length: Int = array.length
    def apply(index: Int): Boolean = array(index)
    def update(index: Int, elem: Boolean) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofBoolean => Arrays.equals(array, that.array)
      case that: ofBoolean1 if length == 1 => array(0) == that.value
      case that: ofBoolean2 if length == 2 => array(0) == that.value1 && array(1) == that.value2
      case _ => super.equals(that)
    }
  }

  final class ofUnit(val array: Array[Unit]) extends WrappedArray[Unit] with Serializable {
    def elemTag = ClassTag.Unit
    def length: Int = array.length
    def apply(index: Int): Unit = array(index)
    def update(index: Int, elem: Unit) { array(index) = elem }
    override def hashCode = MurmurHash3.wrappedArrayHash(array)
    override def equals(that: Any) = that match {
      case that: ofUnit => array.length == that.array.length
      case that: ofUnit1 => length == 1
      case that: ofUnit2 => length == 2
      case _ => super.equals(that)
    }
  }
}

package forCompiler {
  import WrappedArray._
  final class ofRef1[T <: AnyRef](private[mutable] val value: T) extends WrappedArray[T] with Serializable {
    lazy val elemTag = ClassTag[T](value.getClass)
    def length: Int = 1
    def apply(index: Int): T = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: T) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofRef1[_] => value.equals(that.value)
      case that: ofRef[_] if that.length == 1 => value.equals(that.array(0))
      case _ => super.equals(that)
    }
    override lazy val array = Array[T](value) (elemTag)
  }

  final class ofRef2[T <: AnyRef](private[mutable] val value1: T, private[mutable] val value2 :T) extends WrappedArray[T] with Serializable {
    lazy val elemTag = ClassTag[T](value1.getClass)
    def length: Int = 2
    def apply(index: Int): T = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: T) { throw new UnsupportedOperationException("ofRef2 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofRef2[_] => value1.equals(that.value1) && value2.equals(that.value2)
      case that: ofRef[_] if that.length == 2 => value1.equals(that.array(0)) && value2.equals(that.array(1))
      case _ => super.equals(that)
    }
    override lazy val array = Array[T] (value1, value2) (elemTag)
  }

  final class ofByte1(private[mutable] val value: Byte) extends WrappedArray[Byte] with Serializable {
    def elemTag = ClassTag.Byte
    def length: Int = 1
    def apply(index: Int): Byte = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Byte) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofByte1 => value == that.value
      case that: ofByte if that.length == 1 => value == that.array(0)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Byte] { value } (ClassTag.Byte)
  }

  final class ofByte2(private[mutable] val value1: Byte, private[mutable] val value2: Byte) extends WrappedArray[Byte] with Serializable {
    def elemTag = ClassTag.Byte
    def length: Int = 2
    def apply(index: Int): Byte = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Byte) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofByte2 => value1 == that.value1 && value2 == that.value2
      case that: ofByte if that.length == 2 => value1 == that.array(0) && value2 == that.array(1)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Byte](value1, value2) (ClassTag.Byte)
  }

  final class ofShort1(private[mutable] val value: Short) extends WrappedArray[Short] with Serializable {
    def elemTag = ClassTag.Short
    def length: Int = 1
    def apply(index: Int): Short = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Short) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofShort1 => value == that.value
      case _ => super.equals(that)
    }
    override lazy val array = Array[Short] { value } (ClassTag.Short)
  }
  final class ofShort2(private[mutable] val value1: Short, private[mutable] val value2: Short) extends WrappedArray[Short] with Serializable {
    def elemTag = ClassTag.Short
    def length: Int = 2
    def apply(index: Int): Short = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Short) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofShort2 => value1 == that.value1 && value2 == that.value2
      case that: ofShort if that.length == 2 => value1 == that.array(0) && value2 == that.array(1)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Short](value1, value2) (ClassTag.Short)
  }

  final class ofChar1(private[mutable] val value: Char) extends WrappedArray[Char] with Serializable {
    def elemTag = ClassTag.Char
    def length: Int = 1
    def apply(index: Int): Char = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Char) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofChar1 => value == that.value
      case _ => super.equals(that)
    }
    override lazy val array = Array[Char] { value } (ClassTag.Char)
  }
  final class ofChar2(private[mutable] val value1: Char, private[mutable] val value2: Char) extends WrappedArray[Char] with Serializable {
    def elemTag = ClassTag.Char
    def length: Int = 2
    def apply(index: Int): Char = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Char) { throw new UnsupportedOperationException("ofChar2 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofChar2 => value1 == that.value1 && value2 == that.value2
      case that: ofChar if that.length == 2 => value1 == that.array(0) && value2 == that.array(1)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Char](value1, value2) (ClassTag.Char)
  }

  final class ofInt1(private[mutable] val value: Int) extends WrappedArray[Int] with Serializable {
    def elemTag = ClassTag.Int
    def length: Int = 1
    def apply(index: Int): Int = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Int) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofInt1 => value == that.value
      case _ => super.equals(that)
    }
    override lazy val array = Array[Int] { value } (ClassTag.Int)
  }
  final class ofInt2(private[mutable] val value1: Int, private[mutable] val value2: Int) extends WrappedArray[Int] with Serializable {
    def elemTag = ClassTag.Int
    def length: Int = 2
    def apply(index: Int): Int = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Int) { throw new UnsupportedOperationException("ofInt2 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofInt2 => value1 == that.value1 && value2 == that.value2
      case that: ofInt if that.length == 2 => value1 == that.array(0) && value2 == that.array(1)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Int](value1, value2) (ClassTag.Int)
  }

  final class ofLong1(private[mutable] val value: Long) extends WrappedArray[Long] with Serializable {
    def elemTag = ClassTag.Long
    def length: Int = 1
    def apply(index: Int): Long = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Long) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofLong1 => value == that.value
      case _ => super.equals(that)
    }
    override lazy val array = Array[Long] { value } (ClassTag.Long)
  }
  final class ofLong2(private[mutable] val value1: Long, private[mutable] val value2: Long) extends WrappedArray[Long] with Serializable {
    def elemTag = ClassTag.Long
    def length: Int = 2
    def apply(index: Int): Long = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Long) { throw new UnsupportedOperationException("ofLong2 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofLong2 => value1 == that.value1 && value2 == that.value2
      case that: ofLong if that.length == 2 => value1 == that.array(0) && value2 == that.array(1)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Long](value1, value2) (ClassTag.Long)
  }

  final class ofFloat1(private[mutable] val value: Float) extends WrappedArray[Float] with Serializable {
    def elemTag = ClassTag.Float
    def length: Int = 1
    def apply(index: Int): Float = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Float) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofFloat1 => value == that.value
      case _ => super.equals(that)
    }
    override lazy val array = Array[Float] { value } (ClassTag.Float)
  }
  final class ofFloat2(private[mutable] val value1: Float, private[mutable] val value2: Float) extends WrappedArray[Float] with Serializable {
    def elemTag = ClassTag.Float
    def length: Int = 2
    def apply(index: Int): Float = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Float) { throw new UnsupportedOperationException("ofFloat2 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofFloat2 => value1 == that.value1 && value2 == that.value2
      case that: ofFloat if that.length == 2 => value1 == that.array(0) && value2 == that.array(1)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Float](value1, value2) (ClassTag.Float)
  }

  final class ofDouble1(private[mutable] val value: Double) extends WrappedArray[Double] with Serializable {
    def elemTag = ClassTag.Double
    def length: Int = 1
    def apply(index: Int): Double = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Double) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofDouble1 => value == that.value
      case _ => super.equals(that)
    }
    override lazy val array = Array[Double] { value } (ClassTag.Double)
  }

  final class ofDouble2(private[mutable] val value1: Double, private[mutable] val value2: Double) extends WrappedArray[Double] with Serializable {
    def elemTag = ClassTag.Double
    def length: Int = 2
    def apply(index: Int): Double = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Double) { throw new UnsupportedOperationException("ofDouble2 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofDouble2 => value1 == that.value1 && value2 == that.value2
      case that: ofDouble if that.length == 2 => value1 == that.array(0) && value2 == that.array(1)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Double](value1, value2) (ClassTag.Double)
  }

  final class ofBoolean1(private[mutable] val value: Boolean) extends WrappedArray[Boolean] with Serializable {
    def elemTag = ClassTag.Boolean
    def length: Int = 1
    def apply(index: Int): Boolean = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Boolean) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofBoolean1 => value == that.value
      case _ => super.equals(that)
    }
    override lazy val array = Array[Boolean] { value } (ClassTag.Boolean)
  }

  final class ofBoolean2(private[mutable] val value1: Boolean, private[mutable] val value2: Boolean) extends WrappedArray[Boolean] with Serializable {
    def elemTag = ClassTag.Boolean
    def length: Int = 2
    def apply(index: Int): Boolean = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Boolean) { throw new UnsupportedOperationException("ofBoolean2 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofBoolean2 => value1 == that.value1 && value2 == that.value2
      case that: ofBoolean if that.length == 2 => value1 == that.array(0) && value2 == that.array(1)
      case _ => super.equals(that)
    }
    override lazy val array = Array[Boolean](value1, value2) (ClassTag.Boolean)
  }

  final class ofUnit1(private[mutable] val value: Unit) extends WrappedArray[Unit] with Serializable {
    def elemTag = ClassTag.Unit
    def length: Int = 1
    def apply(index: Int): Unit = if (index == 0) value else throw new ArrayIndexOutOfBoundsException(s"only have one element, but access ${index}")
    def update(index: Int, elem: Unit) { throw new UnsupportedOperationException("ofRef1 doesn't support update") }
    override def hashCode = value.hashCode()
    override def equals(that: Any) = that match {
      case that: ofUnit1 => true
      case _ => super.equals(that)
    }
    override lazy val array = Array[Unit] { value } (ClassTag.Unit)
  }

  final class ofUnit2(private[mutable] val value1: Unit, private[mutable] val value2: Unit) extends WrappedArray[Unit] with Serializable {
    def elemTag = ClassTag.Unit
    def length: Int = 2
    def apply(index: Int): Unit = if (index == 0) value1 else if (index == 1) value2 else throw new ArrayIndexOutOfBoundsException(s"only have 2 element, but access ${index}")
    def update(index: Int, elem: Unit) { throw new UnsupportedOperationException("ofUnit2 doesn't support update") }
    override def hashCode = value1.hashCode() + value2.hashCode()
    override def equals(that: Any) = that match {
      case that: ofUnit2 => true
      case that: ofUnit if that.length == 2 => true
      case _ => super.equals(that)
    }
    override lazy val array = Array[Unit](value1, value2) (ClassTag.Unit)
  }

}
