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
import scala.annotation.unchecked.{ uncheckedVariance=> uV }
import scala.collection.immutable.Map.SmallImmutableMap

/** This class implements immutable maps using a hash trie.
 *
 *  '''Note:''' The builder of this hash map may return specialized representations for small maps.
 *
 *  @tparam A      the type of the keys contained in this hash map.
 *  @tparam B      the type of the values associated with the keys.
 *
 *  @author  Martin Odersky
 *  @author  Tiark Rompf
 *  @version 2.8
 *  @since   2.3
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#hash_tries "Scala's Collection Library overview"]]
 *  section on `Hash Tries` for more information.
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
//TODO should this be abstract???
//empty as base class is unsafe for pattern match
@SerialVersionUID(2L)
sealed class HashMap[A, +B] extends AbstractMap[A, B]
                        with Map[A, B]
                        with MapLike[A, B, HashMap[A, B]]
                        with Serializable
{
  import HashMap.{nullToEmpty, bufferSize}

  override def size: Int = 0

  override def empty = HashMap.empty[A, B]

  def iterator: Iterator[(A,B)] = Iterator.empty

  override def foreach[U](f: ((A, B)) => U): Unit = ()

  def get(key: A): Option[B] =
    get0(key, computeHash(key), 0)

  override def contains(key: A): Boolean =
    contains0(key, computeHash(key), 0)

  override def updated [B1 >: B] (key: A, value: B1): HashMap[A, B1] =
    updated0(key, computeHash(key), 0, value, null, null)

  override def + [B1 >: B] (kv: (A, B1)): HashMap[A, B1] =
    updated0(kv._1, computeHash(kv._1), 0, kv._2, kv, null)

  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): HashMap[A, B1] = {
    val builder = new HashMap.HashMapBuilder[A,B1]
    builder += elem1
    builder += elem2
    builder ++= elems
    builder.buildMap
  }

  def - (key: A): HashMap[A, B] =
    removed0(key, computeHash(key), 0)

  override def tail: HashMap[A, B] = this - head._1

  override def filter(p: ((A, B)) => Boolean) = {
    val buffer = new Array[HashMap[A, B]](bufferSize(size))
    nullToEmpty(filter0(p, false, 0, buffer, 0))
  }

  override def filterNot(p: ((A, B)) => Boolean) = {
    val buffer = new Array[HashMap[A, B]](bufferSize(size))
    nullToEmpty(filter0(p, true, 0, buffer, 0))
  }

  protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = null

  protected def elemHashCode(key: A) = key.##

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[collection] def computeHash(key: A) = improve(elemHashCode(key))

  import HashMap.{Merger, MergeFunction, liftMerger}

  private[collection] def get0(key: A, hash: Int, level: Int): Option[B] = None
  protected def contains0(key: A, hash: Int, level: Int): Boolean = false
  private[collection] def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
    new HashMap.HashMap1(key, hash, value, kv)

  protected def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = this

  protected def writeReplace(): AnyRef = new HashMap.SerializationProxy(this)

  def split: Seq[HashMap[A, B]] = Seq(this)

  /** Creates a new map which is the merge of this and the argument hash map.
   *
   *  Uses the specified collision resolution function if two keys are the same.
   *  The collision resolution function will always take the first argument from
   *  `this` hash map and the second from `that`.
   *
   *  The `merged` method is on average more performant than doing a traversal and reconstructing a
   *  new immutable hash map from scratch, or `++`.
   *
   *  @tparam B1      the value type of the other hash map
   *  @param that     the other hash map
   *  @param mergef   the merge function or null if the first key-value pair is to be picked
   */
  def merged[B1 >: B](that: HashMap[A, B1])(mergef: MergeFunction[A, B1]): HashMap[A, B1] = merge0(that, 0, liftMerger(mergef))

  protected def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = that
  override protected[this] def newBuilder: mutable.Builder[(A, B), HashMap[A, B]] = new HashMap.HashMapBuilder[A,B]

  override def keySet = new HashMapKeySet

  /**
    * a marker class that allows more efficient --= from a HashMapBuilder
    */
  final class HashMapKeySet extends ImmutableDefaultKeySet {
    def outer:HashMap[A,B] = HashMap.this
  }

  override def ++[V1 >: B](xs: GenTraversableOnce[(A, V1)]): Map[A, V1] = {
    if (xs.isEmpty) this
    else {
      val builder = new HashMap.HashMapBuilder[A, V1]
      builder addAll this
      builder addAll xs
      builder.result()
    }
  }

  override def --(xs: GenTraversableOnce[A]): HashMap[A, B] = {
    if (xs.isEmpty) this
    else {
      val builder = new HashMap.HashMapBuilder[A, B]
      builder addAll this
      builder removeAll xs
      builder.result()
    }
  }

}

/** $factoryInfo
 *  @define Coll `immutable.HashMap`
 *  @define coll immutable hash map
 *
 *  @author  Tiark Rompf
 *  @since   2.3
 */
object HashMap extends ImmutableMapFactory[HashMap] with BitOperations.Int {

  private[collection] abstract class Merger[A, B] {
    def apply(kv1: (A, B), kv2: (A, B)): (A, B)
    def invert: Merger[A, B]
  }

  private type MergeFunction[A1, B1] = ((A1, B1), (A1, B1)) => (A1, B1)

  private def liftMerger[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] =
    if (mergef == null) defaultMerger.asInstanceOf[Merger[A1, B1]] else liftMerger0(mergef)

  private[this] val defaultMerger : Merger[Any, Any] = liftMerger0((a,b) => a)

  private[this] def liftMerger0[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] = new Merger[A1, B1] {
    self =>
    def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv1, kv2)
    val invert: Merger[A1, B1] = new Merger[A1, B1] {
      def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv2, kv1)
      def invert: Merger[A1, B1] = self
    }
  }
  class TrieMapCanBuildFrom[A, B] extends CanBuildFrom[Coll, (A, B), HashMap[A, B]] {
    def apply(from: Coll) = newBuilder[A, B]
    def apply() = newBuilder[A,B]
  }
  override def newBuilder[A,B] = new HashMapBuilder[A,B]
  /* Start - probably move this to a higher level concern - scala.Map, once the map semantics are agreed   */
  private val javaLangPackage = classOf[java.lang.Integer].getPackage
  private def isBoxedJavaPrimative(c: Class[_]): Boolean = {
    (c.getPackage eq javaLangPackage) && (
      classOf[java.lang.Number].isAssignableFrom(c) || (classOf[java.lang.Character] eq c)
      )
  }
  /**
    * Utility method to detect if the vale of a Map has changed during a += or update action.
    * It is expensive to allocate attional data structures if the value has not changed, but not changed means the same
    * identity, not just ==, so it must be the same JVM instance or be a primative value that matches
    *
    * there is a special case for Double.NaN and Float.NaN as they dont compare equal
    *
    * //TODO - consider AnyVal
    *
    * @param value1 the first value to compare
    * @param value2 the second value to compare
    * @return true if the values ar identical, false otherwise
    */
  private [collection] def identicalValues(value1:Any, value2:Any): Boolean = {
    //    import java.lang.{Double => JDouble}
    val null1 = null == value1
    val null2 = null == value2

    if (null1 || null2) null1 && null2
    else {
      val class1 = value1.getClass
      val class2 = value2.getClass
      if (class1 ne class2) false
      else if (isBoxedJavaPrimative(class1))
      //for boxed primatives NaN == NaN from the same class
      //so we use .equals rather than ==
        value1 equals value2
      else value1.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]
    }
  }
  /* End - probably move this to a higher level concern - scala.Map, once the map semantics are agreed   */
  private[immutable] final class HashMapBuilder[A,B] extends BuilderMutableNode[A,B]
    with mutable.Builder[(A,B), HashMap[A,B]]
    with Shrinkable[A]
    with Map.BuilderOrImmutableMap[A,B] {
    import java.util

    var size = 0
    val data = new Array[BuilderNode[A, B]](32)

    def this(level: Int, newNode: BuilderCollisionNode[A, B]) {
      this()
      val idx = index(level, newNode.hash)
      data(idx) = newNode
      size = newNode.size
    }

    override private [HashMap] def removeBuilt(level: Int, hash: Int, key: A): this.type = {
      val idx = index(level, hash)
      val current = data(idx)
      if (current ne null) {
        val oldSize = current.size
        val newValue = current match {
          case n: BuilderMutableNode[A, B] => n.removeBuilt(level + 5, hash, key)
          case h: HashMap1[A, B] =>
            if (h.hash == hash && h.key == key) null else h
          case t: HashTrieMap[A, B] =>
            if (!t.contains(key)) t //miss
            else if (t.size == 1) null //sole content
            else {
              val builder = new HashMapBuilder[A, B]()
              builder.addHashTrieMap(level, t)
              builder.removeBuilt(level + 5, hash, key)
              builder
            }
          case c: HashMapCollision1[A,B] =>
            if (c.hash != hash) c else {
              val coll = BuilderCollisionNode[A,B](hash)
              coll.addColliding(c)
              coll.removeBuilt(level + 5, hash, key)
              coll
            }
        }

        val newSize = if (newValue eq null) 0 else newValue.size
        size = size + newSize - oldSize
        data(idx) = newValue
      }
      if (level > 0 && size == 0) null else this
    }

    //the upper bound safety of V1 wrt B is maintained by SimpleMapBuilder
    override private[immutable] def buildAdd[V1 >: B](x: (A, V1)):this.type = {
      +=(x.asInstanceOf[(A, B)])
    }

    //the upper bound safety of V1 wrt B is maintained by SimpleMapBuilder
    override private[immutable] def buildAddAll[V1 >: B](xs: TraversableOnce[(A, V1)]):this.type = {
      ++=(xs.asInstanceOf[TraversableOnce[(A, B)]])
    }

    override def result(): HashMap[A, B] = toMap

    //TODO optimise for empty, map1-4?
    override private[immutable] def buildToMap:Map[A,B] = buildMap

    override private[immutable] def buildMap:HashMap[A,B] = toMap

    override def +=(elem: (A, B)): this.type = {
      updateBuilt(0, empty.computeHash(elem._1), elem._1, elem._2, elem)
      this
    }
    private [immutable] def addToBuilder(key:A, value:B): Unit = {
      updateBuilt(0, empty.computeHash(key), key, value, null)
    }
    override def -=(key: A): this.type = {
      removeBuilt(0, empty.computeHash(key), key)
      this
    }

    override def --=(xs: TraversableOnce[A]): this.type = removeAll(xs)
    override def ++=(xs: TraversableOnce[(A, B)]): this.type = addAll(xs)

    def addAll(xs: GenTraversableOnce[(A, B)]): this.type = {
      xs match {
        case hashMap: NonEmptyHashMap[A, B] => addHashMap(0, hashMap)
        case smallMap: SmallImmutableMap[A, B] =>
          // optimised to avoid tuple creation
          smallMap forEachKV addToBuilder
        case _ => xs foreach +=
      }
      this
    }
    def removeAll(xs: GenTraversableOnce[A]): this.type = {
      xs match {
//        case hashMapKeys: HashMapKeySet => removeHashMap(0, hashMapKeys.outer)
        case _ => xs foreach -=
      }
      this
    }

    private def removeHashMap(level:Int, hashMap:HashMap[A,B]) = {
      ??? //TODO
    }

    @inline private def addHashMap(level:Int, hashMap:NonEmptyHashMap[A,B]): HashMapBuilder[A,B] = hashMap match {
      case hashMap: HashTrieMap[A, B] => addHashTrieMap(level, hashMap)
      case hashMap: NonEmptyLeafHashMap[A, B] => addNonEmpty(level, index(level, hashMap.hash), hashMap)
    }


    private def addHashTrieMap(level:Int, hashMap : HashTrieMap[A,B]) :HashMapBuilder[A,B] = {
      val bitmap = hashMap.bitmap
      var realIdx = 0
      var packedIdx = 0
      while (realIdx < 32) {
        val mask = 1 << realIdx
        if ((bitmap & mask ) != 0)
          addNonEmpty2(level, realIdx, hashMap.elems(packedIdx))
        realIdx += 1
      }
      this
    }
    private def addNonEmpty2(level: Int, idx: Int, newNode: HashMap[A, B]) : HashMapBuilder[A,B] = {
      newNode match {
        case bn: NonEmptyHashMap[A, B] => addNonEmpty(level, idx, bn)
        case _ => ???  //todo
      }
    }
    private def addNonEmpty(level: Int, idx: Int, newNode: NonEmptyHashMap[A, B]) : HashMapBuilder[A,B] = {
      val old = data(idx)
      if (old eq null) {
        data(idx) = newNode
        size += newNode.size
      } else if (old eq newNode) {
        // it may look weird to check this, but due to structural sharing
        // adding 2 maps together may have come from a common base and may share structure
        // and its a very quick no-op, and can save a whole tree of comparisons
      } else {
        val oldSize = old.size
        val nextLevel = level + 5
        val newValue = (old, newNode) match {
          case (mutable: HashMapBuilder[A, B], _) =>
            mutable.addHashMap(nextLevel, newNode)
          case (oldMap: HashTrieMap[A, B], _) =>
            new HashMapBuilder[A, B]().addHashTrieMap(nextLevel, oldMap).addHashMap(nextLevel, newNode)
          case (oldLeaf: HashMap1[A, B], newLeaf: HashMap1[A, B]) if oldLeaf.key == newLeaf.key =>
            //special case if the keys of a hashMap1 match
            newLeaf
          case (oldLeaf: NonEmptyLeafHashMap[A, B], newLeaf: NonEmptyLeafHashMap[A, B]) =>
            if (oldLeaf.hash == newLeaf.hash) {
              //hashes are the same but the keys are different, or not HashMap1 so we have a collision
              BuilderCollisionNode[A, B](oldLeaf.hash).addColliding(oldLeaf).addColliding(newLeaf)
            } else {
              //is the hashes are not equal, then we need more tree
              //add a new level and let that level sort it out
              new HashMapBuilder[A, B]().addHashMap(nextLevel, oldLeaf).addHashMap(nextLevel, newNode)
            }
          case (oldLeaf: BuilderCollisionNode[A, B], newLeaf: NonEmptyLeafHashMap[A, B]) =>
            if (oldLeaf.hash == newLeaf.hash) {
              //hashes are the same but the keys are different, so we have a collision
              oldLeaf.addColliding(newLeaf)
            } else {
              //is the hashes are not equal, then we need more tree
              //add a new level and let that level sort it out
              new HashMapBuilder[A, B](nextLevel, oldLeaf).addHashMap(nextLevel, newNode)
            }
        }
        data(idx) = newValue
        size += newValue.size - oldSize
      }
      this
    }

    override def clear(): Unit = if (size > 0) {
      util.Arrays.fill(data.asInstanceOf[Array[AnyRef]], null)
      size = 0
    }
    private def index(level: Int, hash: Int) = (hash >>> level) & 31

    override def updateBuilt(level: Int, hash: Int, key:A, value:B, elemOrNull: (A, B)): HashMapBuilder[A,B] = {
      val idx = index(level, hash)
      val old = data(idx)

      if (old eq null) {
        data(idx) = new HashMap1[A, B](key, hash, value, elemOrNull)
        size += 1
      } else {
        val oldSize = old.size
        val nextLevel = level + 5
        val newValue:BuilderNode[A,B] = old match {
          case mutable: BuilderMutableNode[A, B] =>
            mutable.updateBuilt(nextLevel, hash, key, value, elemOrNull)
          case map1: NonEmptyLeafHashMap[A, B] if (map1.hash != hash) =>
            //add a new level and let that level sort it out
            new HashMapBuilder[A, B].addHashMap(nextLevel, map1).updateBuilt(nextLevel, hash, key, value, elemOrNull)

          case map1: HashMap1[A, B] =>
            if (map1.key == key) {
              //retain the old map1 to reduce garbage where possible
              if (identicalValues(value, map1.value)) map1
              else new HashMap1[A, B](key, hash, value, elemOrNull)
            } else {
              val elem = if (elemOrNull eq null) (key, value) else elemOrNull
              val newNode = BuilderCollisionNode[A, B](hash)
              newNode.addColliding(map1.ensurePair)
              newNode.addColliding(elem)
              newNode
            }
          case collision: HashMapCollision1[A,B] =>
            val res = BuilderCollisionNode[A,B](hash, collision.kvs)
            res.addColliding(if (elemOrNull eq null) (key, value) else elemOrNull)
            res
        }
        data(idx) = newValue
        size += newValue.size - oldSize
      }
      this
    }


    def toMap: HashMap[A, B] = {
      var readIndex = 0
      var lastReadIndex: Int = -1
      var slotsUsed = 0
      while (readIndex < 32) {
        val current = data(readIndex)
        if ((current ne null) && current.size > 0) {
          lastReadIndex = readIndex
          slotsUsed += 1
        }
        readIndex += 1
      }
      if (slotsUsed < 2) {
        //no slots used
        if (lastReadIndex == -1) empty[A,B]
        else data(lastReadIndex).buildMap match {
          case m1:HashMap1[A,B] => m1
          case c1:HashMapCollision1[A,B] => c1
          case t:HashTrieMap[A,B] =>
            //we can't collapse multiple levels of TrieMaps
            // TODO consider a more optimal structure without the array
            // a sort of 'single node TrieMap'
            new HashTrieMap[A, B](1 << lastReadIndex, Array(t), t.size)
            // only empty is left
          case _: HashMap.EmptyHashMap.type => empty[A,B]
          case _: HashMap[_,_] => ??? //TODO HashMap should be abstract

        }
      } else {
        val newData = new Array[HashMap[A, B]](slotsUsed)
        var readIndex = 0
        var writeIndex = 0
        var bitmap = 0
        while (readIndex < 32) {
          val read = data(readIndex)
          if ((read ne null) && read.size > 0) {
            newData(writeIndex) = read.buildMap
            writeIndex += 1
            bitmap |= 1 << readIndex
          }
          readIndex += 1
        }
        assert(writeIndex == newData.length)
        new HashTrieMap[A, B](bitmap, newData, size)
      }
    }
  }
  //a mutable collision node
  private[HashMap] object BuilderCollisionNode{
    def apply[A,B] (hash:Int, initialValues: ListMap[A,B]): BuilderCollisionNode[A,B] ={
      val res = new BuilderCollisionNode[A,B](hash)
      res.data ++= initialValues
      //TODO consider reuseIndex and reusedBuiltTail
      res
    }
    def apply[A,B] (hash:Int): BuilderCollisionNode[A,B] ={
      new BuilderCollisionNode[A,B](hash)
    }
  }
  private[HashMap] final class BuilderCollisionNode[A,B] private (private[HashMap] val hash:Int) extends BuilderMutableNode[A,B] with BuilderLeafNode[A,B]{
    private val data = new scala.collection.mutable.ArrayBuffer[(A,B)]
    /**
      * the [[reusedBuiltTail]].size -1. elements <= this index in [[data]] are shared to [[reusedBuiltTail]]
      */
    private var reuseIndex:Int = -1
    /**
      * a prebuild map of the head [[reuseIndex]] elements from data
      * this is maintained to promote reuse
      */
    private var reusedBuiltTail: ListMap[A,B] = ListMap.empty

    def makeMap = {
//      for (idx <- reuseIndex + 1 until data.size ) {
      // for the moment we will ignore reuse index and reusedBuiltTail
      // and rebuild from scratch
      //TODO re-enable use of reusedBuiltTail when we have more test coverage
      reusedBuiltTail = ListMap.empty
      for (idx <- 0 until data.size ) {
        val kv = data(idx)
        reusedBuiltTail = reusedBuiltTail.addNoCheck(kv._1, kv._2)
      }
      reuseIndex = data.size -1
      reusedBuiltTail
    }

    override private[HashMap] def removeBuilt(level: Int, hash: Int, key: A) = {
      if (hash == this.hash) {
        var idx = 0
        val max = data.size
        var found = false
        while (idx < max && !found) {
          val value = data(idx)
          if (value._1 == key) {
            data.remove(idx)
            found = true
          } else idx += 1
        }
        if (found) {
          reusedBuiltTail = reusedBuiltTail.dropLast(reuseIndex - idx)
          reuseIndex = reuseIndex - idx
        }
      }
      this
    }

    override def buildMap: HashMap[A, B] = new HashMapCollision1[A,B](hash, makeMap)
    override def updateBuilt(level: Int, hash: Int, key:A, value:B, elemOrNull: (A, B)): BuilderNode[A,B] = {
      if (this.hash == hash)
        addColliding( if (elemOrNull eq null) (key, value) else elemOrNull)
      else
        new HashMapBuilder[A,B](level, this).updateBuilt(level, hash, key, value, elemOrNull)
    }
    def addColliding(nonEmptyLeafHashMap: NonEmptyLeafHashMap[A,B]): BuilderCollisionNode[A, B] = {
      addCollidingImpl(nonEmptyLeafHashMap, data.isEmpty)
    }
    private def addCollidingImpl(nonEmptyLeafHashMap: NonEmptyLeafHashMap[A,B], isDistinct:Boolean): BuilderCollisionNode[A, B] = {
      nonEmptyLeafHashMap match {
        case collision: HashMapCollision1[A, B] =>
          reusedBuiltTail = collision.kvs
          data ++= reusedBuiltTail
          // reverse the order - we want to have index 0 as the head of the list
          // for reuse
          var start = 0
          var end = data.size -1
          while (start < end ) {
            val temp = data(start)
            data(start) = data(end)
            data(end) = temp
            start += 1
            end -= 1
          }
        case _ => nonEmptyLeafHashMap foreach (addCollidingImpl(_, isDistinct))
      }
      this
    }
    def addColliding(kv:(A,B)): BuilderCollisionNode[A, B] = {
      addCollidingImpl(kv, data.isEmpty)
    }
    private def addCollidingImpl(kv:(A,B) , isDistinct:Boolean): BuilderCollisionNode[A, B] = {
      if (isDistinct) {
        data += kv
      } else {
        var idx = 0
        var found = false
        val max = data.size
        while (idx < max && !found) {
          val value = data(idx)
          if (value._1 == kv._1) {
            found = true
            if (!identicalValues(value._2, kv._2)) {
              data(idx) = kv
            }
          } else idx += 1
          if (idx == max)
            data += kv
        }
//          while (idx < reuseIndex) {
//            reuseIndex -= 1
//            reusedBuiltTail = reusedBuiltTail.init
//          }
//        }
      }
      this
    }

    override def size = data.size
  }
  sealed trait BuilderNode[A,+B] {
    private[HashMap] def buildMap : HashMap[A, B]
    //for the BuilderImmutableLeafNode size is already inherited, so we can use this global scope
    def size:Int
  }
  sealed trait BuilderLeafNode[A,+B] extends BuilderNode[A,B] {
    private[HashMap] val hash:Int
  }
  sealed abstract class NonEmptyHashMap[A,+B] extends HashMap[A,B] with BuilderNode[A,B] {
    self : HashMap[A,B] =>
    final private[HashMap] def buildMap = self
  }
  sealed trait NonEmptyLeafHashMap[A,+B] extends NonEmptyHashMap[A,B] with BuilderLeafNode[A,B]

  sealed trait BuilderMutableNode[A,B] extends BuilderNode[A,B] {
    private [HashMap] def updateBuilt(level: Int, hash: Int, key:A, value:B, elemOrNull: (A, B)): BuilderNode[A, B]
    private [HashMap] def removeBuilt(level: Int, hash: Int, key:A): BuilderNode[A, B]
  }

  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), HashMap[A, B]] = new TrieMapCanBuildFrom[A, B]
  def empty[A, B]: HashMap[A, B] = EmptyHashMap.asInstanceOf[HashMap[A, B]]

  private object EmptyHashMap extends HashMap[Any, Nothing] { 
    override def head: (Any, Nothing) = throw new NoSuchElementException("Empty Map")
    override def tail: HashMap[Any, Nothing] = throw new NoSuchElementException("Empty Map")
    //override to avoid hashcode calculation
    override def get(key: Any): Option[Nothing] = None
    override def contains(key: Any): Boolean = false
  }

  // utility method to create a HashTrieMap from two leaf HashMaps (HashMap1 or HashMapCollision1) with non-colliding hash code)
  private def makeHashTrieMap[A, B](hash0:Int, elem0:HashMap[A, B], hash1:Int, elem1:HashMap[A, B], level:Int, size:Int) : HashTrieMap[A, B] = {
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[HashMap[A,B]](2)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieMap[A, B](bitmap, elems, size)
    } else {
      val elems = new Array[HashMap[A,B]](1)
      val bitmap = (1 << index0)
      elems(0) = makeHashTrieMap(hash0, elem0, hash1, elem1, level + 5, size)
      new HashTrieMap[A, B](bitmap, elems, size)
    }
  }

  @deprecatedInheritance("This class will be made final in a future release.", "2.12.2")
  class HashMap1[A,+B](private[collection] val key: A, private[collection] val hash: Int, private[collection] val value: (B @uV), private[collection] var kv: (A,B @uV)) extends NonEmptyLeafHashMap[A,B] {
    override def size = 1

    private[collection] def getKey = key
    private[collection] def getHash = hash
    private[collection] def computeHashFor(k: A) = computeHash(k)

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash && key == this.key) Some(value) else None

    override protected def contains0(key: A, hash: Int, level: Int): Boolean =
      hash == this.hash && key == this.key
    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
      if (hash == this.hash && key == this.key ) {
        if (merger eq null) {
          if (this.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this
          else new HashMap1(key, hash, value, kv)
        } else {
          val nkv = merger(this.ensurePair, if(kv != null) kv else (key, value))
          new HashMap1(nkv._1, hash, nkv._2, nkv)
        }
      } else {
        if (hash != this.hash) {
          // they have different hashes, but may collide at this level - find a level at which they don't
          val that = new HashMap1[A, B1](key, hash, value, kv)
          makeHashTrieMap[A,B1](this.hash, this, hash, that, level, 2)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashMapCollision1(hash, ListMap.empty.updated(this.key,this.value).updated(key,value))
        }
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] =
      if (hash == this.hash && key == this.key) HashMap.empty[A,B] else this

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] =
      if (negate ^ p(ensurePair)) this else null

    override def iterator: Iterator[(A,B)] = Iterator(ensurePair)
    override def foreach[U](f: ((A, B)) => U): Unit = f(ensurePair)
    // this method may be called multiple times in a multithreaded environment, but that's ok
    private[HashMap] def ensurePair: (A,B) = if (kv ne null) kv else { kv = (key, value); kv }
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = {
      that.updated0(key, hash, level, value, kv, merger.invert)
    }
  }

  private[collection] class HashMapCollision1[A, +B](private[collection] val hash: Int, val kvs: ListMap[A, B @uV])
          extends NonEmptyLeafHashMap[A,B] {
    // assert(kvs.size > 1)

    override def size = kvs.size

    override def get0(key: A, hash: Int, level: Int): Option[B] =
      if (hash == this.hash) kvs.get(key) else None

    override protected def contains0(key: A, hash: Int, level: Int): Boolean =
      hash == this.hash && kvs.contains(key)

    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] =
      if (hash == this.hash) {
        if ((merger eq null) || !kvs.contains(key)) new HashMapCollision1(hash, kvs.updated(key, value))
        else new HashMapCollision1(hash, kvs + merger((key, kvs(key)), kv))
      } else {
        val that = new HashMap1(key, hash, value, kv)
        makeHashTrieMap(this.hash, this, hash, that, level, size + 1)
      }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] =
      if (hash == this.hash) {
        val kvs1 = kvs - key
        kvs1.size match {
          case 0 =>
            HashMap.empty[A,B]
          case 1 =>
            val kv = kvs1.head
            new HashMap1(kv._1,hash,kv._2,kv)
          case x if x == kvs.size =>
            this
          case _ =>
            new HashMapCollision1(hash, kvs1)
        }
      } else this

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = {
      val kvs1 = if(negate) kvs.filterNot(p) else kvs.filter(p)
      kvs1.size match {
        case 0 =>
          null
        case 1 =>
          val kv@(k,v) = kvs1.head
          new HashMap1(k, hash, v, kv)
        case x if x == kvs.size =>
          this
        case _ =>
          new HashMapCollision1(hash, kvs1)
      }
    }

    override def iterator: Iterator[(A,B)] = kvs.iterator
    override def foreach[U](f: ((A, B)) => U): Unit = kvs.foreach(f)
    override def split: Seq[HashMap[A, B]] = {
      val (x, y) = kvs.splitAt(kvs.size / 2)
      def newhm(lm: ListMap[A, B @uV]) = new HashMapCollision1(hash, lm)
      List(newhm(x), newhm(y))
    }
    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = {
      // this can be made more efficient by passing the entire ListMap at once
      var m = that
      for (p <- kvs) m = m.updated0(p._1, this.hash, level, p._2, p, merger)
      m
    }
  }

  @deprecatedInheritance("This class will be made final in a future release.", "2.12.2")
  class HashTrieMap[A, +B](
    private[collection] val bitmap: Int,
    private[collection] val elems: Array[HashMap[A, B @uV]],
    private[collection] val size0: Int
  ) extends NonEmptyHashMap[A, B @uV] {

    // assert(Integer.bitCount(bitmap) == elems.length)
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieMap[_,_]]))

    override def size = size0

    override def get0(key: A, hash: Int, level: Int): Option[B] = {
      // Note: this code is duplicated with `contains0`
      val index = (hash >>> level) & 0x1f
      if (bitmap == - 1) {
        elems(index).get0(key, hash, level + 5)
      } else {
        val mask = (1 << index)
        if ((bitmap & mask) != 0) {
          val offset = Integer.bitCount(bitmap & (mask - 1))
          elems(offset).get0(key, hash, level + 5)
        } else {
          None
        }
      }
    }

    override protected def contains0(key: A, hash: Int, level: Int): Boolean = {
      // Note: this code is duplicated from `get0`
      val index = (hash >>> level) & 0x1f
      if (bitmap == - 1) {
        elems(index).contains0(key, hash, level + 5)
      } else {
        val mask = (1 << index)
        if ((bitmap & mask) != 0) {
          val offset = Integer.bitCount(bitmap & (mask - 1))
          elems(offset).contains0(key, hash, level + 5)
        } else {
          false
        }
      }
    }

    private[collection] override def updated0[B1 >: B](key: A, hash: Int, level: Int, value: B1, kv: (A, B1), merger: Merger[A, B1]): HashMap[A, B1] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5, value, kv, merger)
        if(subNew eq sub) this else {
          val elemsNew = new Array[HashMap[A,B1]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
      } else {
        val elemsNew = new Array[HashMap[A,B1]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashMap1(key, hash, value, kv)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        new HashTrieMap(bitmap | mask, elemsNew, size + 1)
      }
    }

    override def removed0(key: A, hash: Int, level: Int): HashMap[A, B] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (subNew eq sub) this
        else if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashMap[A,B]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // HashSet1 or HashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieMap[_,_]])
              elemsNew(0)
            else
              new HashTrieMap(bitmapNew, elemsNew, sizeNew)
          } else
            HashMap.empty[A,B]
        } else if(elems.length == 1 && !subNew.isInstanceOf[HashTrieMap[_,_]]) {
          subNew
        } else {
          val elemsNew = new Array[HashMap[A,B]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieMap(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override protected def filter0(p: ((A, B)) => Boolean, negate: Boolean, level: Int, buffer: Array[HashMap[A, B @uV]], offset0: Int): HashMap[A, B] = {
      // current offset
      var offset = offset0
      // result size
      var rs = 0
      // bitmap for kept elems
      var kept = 0
      // loop over all elements
      var i = 0
      while (i < elems.length) {
        val result = elems(i).filter0(p, negate, level + 5, buffer, offset)
        if (result ne null) {
          buffer(offset) = result
          offset += 1
          // add the result size
          rs += result.size
          // mark the bit i as kept
          kept |= (1 << i)
        }
        i += 1
      }
      if (offset == offset0) {
        // empty
        null
      } else if (rs == size0) {
        // unchanged
        this
      } else if (offset == offset0 + 1 && !buffer(offset0).isInstanceOf[HashTrieMap[A, B]]) {
        // leaf
        buffer(offset0)
      } else {
        // we have to return a HashTrieMap
        val length = offset - offset0
        val elems1 = new Array[HashMap[A, B]](length)
        System.arraycopy(buffer, offset0, elems1, 0, length)
        val bitmap1 = if (length == elems.length) {
          // we can reuse the original bitmap
          bitmap
        } else {
          // calculate new bitmap by keeping just bits in the kept bitmask
          keepBits(bitmap, kept)
        }
        new HashTrieMap(bitmap1, elems1, rs)
      }
    }

    override def iterator: Iterator[(A, B)] = new TrieIterator[(A, B)](elems.asInstanceOf[Array[Iterable[(A, B)]]]) {
      final override def getElem(cc: AnyRef): (A, B) = cc.asInstanceOf[HashMap1[A, B]].ensurePair
    }

    override def foreach[U](f: ((A, B)) => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }

    private def posOf(n: Int, bm: Int) = {
      var left = n
      var i = -1
      var b = bm
      while (left >= 0) {
        i += 1
        if ((b & 1) != 0) left -= 1
        b = b >>> 1
      }
      i
    }

    override def split: Seq[HashMap[A, B]] = if (size == 1) Seq(this) else {
      val nodesize = Integer.bitCount(bitmap)
      if (nodesize > 1) {
        val splitpoint = nodesize / 2
        val bitsplitpoint = posOf(nodesize / 2, bitmap)
        val bm1 = bitmap & (-1 << bitsplitpoint)
        val bm2 = bitmap & (-1 >>> (32 - bitsplitpoint))

        val (e1, e2) = elems.splitAt(splitpoint)
        val hm1 = new HashTrieMap(bm1, e1, e1.foldLeft(0)(_ + _.size))
        val hm2 = new HashTrieMap(bm2, e2, e2.foldLeft(0)(_ + _.size))

        List(hm1, hm2)
      } else elems(0).split
    }

    protected override def merge0[B1 >: B](that: HashMap[A, B1], level: Int, merger: Merger[A, B1]): HashMap[A, B1] = that match {
      case hm: HashMap1[_, _] =>
        this.updated0(hm.key, hm.hash, level, hm.value.asInstanceOf[B1], hm.kv, merger)
      case hm: HashTrieMap[_, _] =>
        val that = hm.asInstanceOf[HashTrieMap[A, B1]]
        val thiselems = this.elems
        val thatelems = that.elems
        var thisbm = this.bitmap
        var thatbm = that.bitmap

        // determine the necessary size for the array
        val subcount = Integer.bitCount(thisbm | thatbm)

        // construct a new array of appropriate size
        val merged = new Array[HashMap[A, B1]](subcount)

        // run through both bitmaps and add elements to it
        var i = 0
        var thisi = 0
        var thati = 0
        var totalelems = 0
        while (i < subcount) {
          val thislsb = thisbm ^ (thisbm & (thisbm - 1))
          val thatlsb = thatbm ^ (thatbm & (thatbm - 1))

          // collision
          if (thislsb == thatlsb) {
            val m = thiselems(thisi).merge0(thatelems(thati), level + 5, merger)
            totalelems += m.size
            merged(i) = m
            thisbm = thisbm & ~thislsb
            thatbm = thatbm & ~thatlsb
            thati += 1
            thisi += 1
          } else {
            // condition below is due to 2 things:
            // 1) no unsigned int compare on JVM
            // 2) 0 (no lsb) should always be greater in comparison
            if (unsignedCompare(thislsb - 1, thatlsb - 1)) {
              val m = thiselems(thisi)
              totalelems += m.size
              merged(i) = m
              thisbm = thisbm & ~thislsb
              thisi += 1
            }
            else {
              val m = thatelems(thati)
              totalelems += m.size
              merged(i) = m
              thatbm = thatbm & ~thatlsb
              thati += 1
            }
          }
          i += 1
        }

        new HashTrieMap[A, B1](this.bitmap | that.bitmap, merged, totalelems)
      case hm: HashMapCollision1[_, _] => that.merge0(this, level, merger.invert)
      case hm: HashMap[_, _] => this
      case _ => throw new IllegalStateException("section supposed to be unreachable.")
    }
  }

  /**
   * Calculates the maximum buffer size given the maximum possible total size of the trie-based collection
   * @param size the maximum size of the collection to be generated
   * @return the maximum buffer size
   */
  @inline private def bufferSize(size: Int): Int = (size + 6) min (32 * 7)

  /**
   * In many internal operations the empty map is represented as null for performance reasons. This method converts
   * null to the empty map for use in public methods
   */
  @inline private def nullToEmpty[A, B](m: HashMap[A, B]): HashMap[A, B] = if (m eq null) empty[A, B] else m

  /**
   * Utility method to keep a subset of all bits in a given bitmap
   *
   * Example
   *    bitmap (binary): 00000001000000010000000100000001
   *    keep (binary):                               1010
   *    result (binary): 00000001000000000000000100000000
   *
   * @param bitmap the bitmap
   * @param keep a bitmask containing which bits to keep
   * @return the original bitmap with all bits where keep is not 1 set to 0
   */
  private def keepBits(bitmap: Int, keep: Int): Int = {
    var result = 0
    var current = bitmap
    var kept = keep
    while (kept != 0) {
      // lowest remaining bit in current
      val lsb = current ^ (current & (current - 1))
      if ((kept & 1) != 0) {
        // mark bit in result bitmap
        result |= lsb
      }
      // clear lowest remaining one bit in abm
      current &= ~lsb
      // look at the next kept bit
      kept >>>= 1
    }
    result
  }

  @SerialVersionUID(2L)
  private class SerializationProxy[A,B](@transient private var orig: HashMap[A, B]) extends Serializable {
    private def writeObject(out: java.io.ObjectOutputStream) {
      val s = orig.size
      out.writeInt(s)
      for ((k,v) <- orig) {
        out.writeObject(k)
        out.writeObject(v)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      orig = empty
      val s = in.readInt()
      for (i <- 0 until s) {
        val key = in.readObject().asInstanceOf[A]
        val value = in.readObject().asInstanceOf[B]
        orig = orig.updated(key, value)
      }
    }

    private def readResolve(): AnyRef = orig
  }
}
