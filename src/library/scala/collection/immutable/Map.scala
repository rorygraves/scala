/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package immutable

import generic._
import scala.collection.mutable

/**
 * A generic trait for immutable maps. Concrete classes have to provide
 * functionality for the abstract methods in `Map`:
 *
 * {{{
 *    def get(key: K): Option[V]
 *    def iterator: Iterator[(K, V)]
 *    def + [V1 >: V](kv: (K, V1)): Map[K, V1]
 *    def -(key: K): Map[K, V]
 * }}}
 *
 * @since 1
 */
trait Map[K, +V] extends Iterable[(K, V)]
//                    with GenMap[K, V]
                    with scala.collection.Map[K, V]
                    with MapLike[K, V, Map[K, V]] { self =>

  override def empty: Map[K, V] = Map.empty

  /** Returns this $coll as an immutable map.
   *
   *  A new map will not be built; lazy collections will stay lazy.
   */
  @deprecatedOverriding("Immutable maps should do nothing on toMap except return themselves cast as a map.",  "2.11.0")
  override def toMap[T, U](implicit ev: (K, V) <:< (T, U)): immutable.Map[T, U] =
    self.asInstanceOf[immutable.Map[T, U]]

  override def seq: Map[K, V] = this

  /** The same map with a given default function.
   *  Note: `get`, `contains`, `iterator`, `keys`, etc are not affected by `withDefault`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     the function mapping keys to values, used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefault[V1 >: V](d: K => V1): immutable.Map[K, V1] = new Map.WithDefault[K, V1](this, d)

  /** The same map with a given default value.
   *  Note: `get`, `contains`, `iterator`, `keys`, etc are not affected by `withDefaultValue`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     default value used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefaultValue[V1 >: V](d: V1): immutable.Map[K, V1] = new Map.WithDefault[K, V1](this, x => d)

  /** Add a key/value pair to this map.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   */
  override def updated [V1 >: V](key: K, value: V1): Map[K, V1]
  def + [V1 >: V](kv: (K, V1)): Map[K, V1]
}

/** $factoryInfo
 *  @define Coll `immutable.Map`
 *  @define coll immutable map
 */
object Map extends ImmutableMapFactory[Map] {

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), Map[K, V]] = new MapCanBuildFrom[K,V]

  override def newBuilder[K,V]: mutable.Builder[(K,V), Map[K,V]] = new SimpleMapBuilder[K,V] (emptyForBuilder)
  private[immutable] trait BuilderOrImmutableMap[K,+V] {
     private[immutable] def buildAdd[V1 >:V](x: (K, V1)): BuilderOrImmutableMap[K,V1]
     private[immutable] def buildAddAll[V1 >:V](xs: TraversableOnce[(K, V1)]): BuilderOrImmutableMap[K,V1]
     private[immutable] def buildToMap : Map[K,V]
  }
  private[immutable] sealed abstract class SmallImmutableMap[K,+V] extends AbstractMap[K,V] with Serializable with BuilderOrImmutableMap[K,V]  {
    override private[immutable] def buildToMap: Map[K, V] = this
    private[immutable] def forEachKV (fn : (K,V) => Unit)
    override def ++[V1 >: V](xs: GenTraversableOnce[(K, V1)]): Map[K, V1] = {
      xs match {
        case m: HashMap[K,V1] =>
          // if we are adding a large map to this small map then best to start with the large map
          this.foldLeft (m:Map[K,V1]) {
            case (map,(k,v)) =>
              if (m contains k) m else m.updated(k,v)
          }
        case _ => super.++(xs)
      }
    }

    override def --(xs: GenTraversableOnce[K]): Map[K, V] =
      xs match {
        case s: AbstractSet[K] if s.sizeHintIfCheap > size * 2 =>
          // if we are removing a large keyset from this small map then to look for elements in the large set
          this.foldLeft (this: Map[K, V]) {
            case (map, (k,_) ) =>
              if (s contains k) map - k else map
          }
        case _ => super.--(xs)
      }

    override private[immutable] def buildAddAll[V1 >: V](xs: TraversableOnce[(K, V1)]): BuilderOrImmutableMap[K,V1] = {
      xs match {
        case m: HashMap[K,V1] =>
          val builder = new HashMap.HashMapBuilder[K, V1]
          builder ++= this
          builder ++= m
          builder
        case _ => xs.foldLeft(this: BuilderOrImmutableMap[K, V1]) {
          case (map, (k,v)) =>
            map.buildAdd(k,v)
        }
      }
    }
  }

  private class SimpleMapBuilder[K, V](initial: BuilderOrImmutableMap[K, V]) extends mutable.ReusableBuilder[(K, V), Map[K,V]] {
    protected var elems: BuilderOrImmutableMap[K, _ <: V] = initial
    override def +=(x: (K, V)): this.type = {elems = elems buildAdd x; this}

    override def ++=(xs: TraversableOnce[(K, V)]): this.type = {elems = elems buildAddAll xs; this}

    def clear() { elems = emptyForBuilder }
    def result: Map[K,V] = elems.buildToMap
  }

  private[immutable] def emptyForBuilder[K,V]: BuilderOrImmutableMap[K,V] = EmptyMap.asInstanceOf[BuilderOrImmutableMap[K, V]]
  def empty[K, V]: Map[K, V] = EmptyMap.asInstanceOf[Map[K, V]]

  class WithDefault[K, +V](underlying: Map[K, V], d: K => V) extends scala.collection.Map.WithDefault[K, V](underlying, d) with Map[K, V] {
    override def empty = new WithDefault(underlying.empty, d)
    override def updated[V1 >: V](key: K, value: V1): WithDefault[K, V1] = new WithDefault[K, V1](underlying.updated[V1](key, value), d)
    override def + [V1 >: V](kv: (K, V1)): WithDefault[K, V1] = updated(kv._1, kv._2)
    override def - (key: K): WithDefault[K, V] = new WithDefault(underlying - key, d)
    override def withDefault[V1 >: V](d: K => V1): immutable.Map[K, V1] = new WithDefault[K, V1](underlying, d)
    override def withDefaultValue[V1 >: V](d: V1): immutable.Map[K, V1] = new WithDefault[K, V1](underlying, x => d)
  }

  private object EmptyMap extends AbstractMap[Any, Nothing] with Map[Any, Nothing] with Serializable with BuilderOrImmutableMap[Any, Any] {
    override def size: Int = 0
    override def apply(key: Any) = throw new NoSuchElementException("key not found: " + key)
    override def contains(key: Any) = false
    def get(key: Any): Option[Nothing] = None
    def iterator: Iterator[(Any, Nothing)] = Iterator.empty
    override def updated [V1] (key: Any, value: V1): Map[Any, V1] =  new Map1(key, value)
    def + [V1](kv: (Any, V1)): Map[Any, V1] = updated(kv._1, kv._2)
    override def ++[V1 >: Nothing](xs: GenTraversableOnce[(Any, V1)]): Map[Any, V1] = {
      xs match {
        case m: HashMap[Any,V1] => m
        case m: SmallImmutableMap[Any,V1] => m
        case _ => super.++(xs)
      }
    }
    def - (key: Any): Map[Any, Nothing] = this


    override private[immutable] def buildAdd[V1 >: Any](x: (Any, V1)): BuilderOrImmutableMap[Any, V1] = new Map1[Any,V1](x._1, x._2)
    override private[immutable] def buildAddAll[V1 >: Any](xs: TraversableOnce[(Any, V1)]): BuilderOrImmutableMap[Any, V1] = {
      xs match {
        case m: HashMap[Any,V1] => 
          val builder = new HashMap.HashMapBuilder[Any, Any]
          builder ++= m
          builder
        case m: SmallImmutableMap[Any,Any] => m
        case _ => xs.foldLeft(this: BuilderOrImmutableMap[Any, Any]) {
          case (map, (k,v)) => 
            map.buildAdd(k,v)
        }
      }
    }
    override private[immutable] def buildToMap: Map[Any, Any] = this
  }

  class Map1[K, +V](key1: K, value1: V) extends SmallImmutableMap[K, V] {
    override def size = 1
    override def apply(key: K) = if (key == key1) value1 else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = key == key1
    def get(key: K): Option[V] =
      if (key == key1) Some(value1) else None
    def iterator = Iterator((key1, value1))
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] =
      updatedImpl(key,value)
    private def updatedImpl [V1 >: V] (key: K, value: V1) : SmallImmutableMap[K, V1] =
      if (key == key1)
        if (value == value1) this else new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updatedImpl(kv._1, kv._2)
    def - (key: K): Map[K, V] =
      if (key == key1) Map.empty else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1))
    }
    override private[immutable] def forEachKV(f: (K, V) => Unit) = {
      f(key1, value1)
    }

    override private[immutable] def buildAdd[V1 >: V](x: (K, V1)): BuilderOrImmutableMap[K, V1] = updatedImpl(x._1, x._2)
  }

  class Map2[K, +V](key1: K, value1: V, key2: K, value2: V) extends SmallImmutableMap[K, V]  {
    override def size = 2
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else None
    def iterator = Iterator((key1, value1), (key2, value2))
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] = updatedImpl(key,value)
    private def updatedImpl [V1 >: V] (key: K, value: V1): SmallImmutableMap[K, V1] =
      if (key == key1)
        if (value == value1) this else new Map2(key1, value, key2, value2)
      else if (key == key2)
        if (value == value2) this else new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updatedImpl(kv._1, kv._2)
    def - (key: K): Map[K, V] =
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2))
    }

    override private[immutable] def forEachKV(f: (K, V) => Unit) = {
      f(key1, value1); f(key2, value2)
    }
    override private[immutable] def buildAdd[V1 >: V](x: (K, V1)): BuilderOrImmutableMap[K, V1] = updatedImpl(x._1, x._2)
  }

  class Map3[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V) extends SmallImmutableMap[K, V] {
    override def size = 3
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2) || (key == key3)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else None
    def iterator = Iterator((key1, value1), (key2, value2), (key3, value3))
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] = updatedImpl(key,value)
    private def updatedImpl [V1 >: V] (key: K, value: V1): SmallImmutableMap[K, V1] =
      if (key == key1)
        if (value == value1) this else new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2)
        if (value == value2) this else new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3)
        if (value == value3) this else new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updatedImpl(kv._1, kv._2)
    def - (key: K): Map[K, V] =
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
    override private[immutable] def forEachKV(f: (K, V) => Unit) = {
      f(key1, value1); f(key2, value2); f(key3, value3)
    }
    override private[immutable] def buildAdd[V1 >: V](x: (K, V1)): BuilderOrImmutableMap[K, V1] = updatedImpl(x._1, x._2)
  }

  class Map4[K, +V](key1: K, value1: V, key2: K, value2: V, key3: K, value3: V, key4: K, value4: V) extends SmallImmutableMap[K, V] {
    override def size = 4
    override def apply(key: K) =
      if (key == key1) value1
      else if (key == key2) value2
      else if (key == key3) value3
      else if (key == key4) value4
      else throw new NoSuchElementException("key not found: " + key)
    override def contains(key: K) = (key == key1) || (key == key2) || (key == key3) || (key == key4)
    def get(key: K): Option[V] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else if (key == key4) Some(value4)
      else None
    def iterator = Iterator((key1, value1), (key2, value2), (key3, value3), (key4, value4))
    override def updated [V1 >: V] (key: K, value: V1): Map[K, V1] = updatedImpl(key,value).buildToMap
    private def updatedImpl [V1 >: V] (key: K, value: V1): BuilderOrImmutableMap[K, V1] =
      if (key == key1)
        if (value == value1) this else new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2)
        if (value == value2) this else new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3)
        if (value == value3) this else new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4)
        if (value == value4) this else new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else {
        val builder = new HashMap.HashMapBuilder[K,V1]()
        builder ++= this
        builder.addToBuilder(key,value)
        builder
      }
    def + [V1 >: V](kv: (K, V1)): Map[K, V1] = updated(kv._1, kv._2)
    def - (key: K): Map[K, V] =
      if (key == key1)      new Map3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach[U](f: ((K, V)) => U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
    override private[immutable] def forEachKV(f: (K, V) => Unit) = {
      f(key1, value1); f(key2, value2); f(key3, value3); f(key4, value4)
    }

    override private[immutable] def buildAdd[V1 >: V](x: (K, V1)): BuilderOrImmutableMap[K, V1] = updatedImpl(x._1, x._2)

  }
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[K, +V] extends scala.collection.AbstractMap[K, V] with Map[K, V]
