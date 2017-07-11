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
import scala.annotation.tailrec

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list map with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#list_maps "Scala's Collection Library overview"]]
  * section on `List Maps` for more information.
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  */
object ListMap extends ImmutableMapFactory[ListMap] {

  /**
    * $mapCanBuildFromInfo
    */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ListMap[A, B]] =
    new MapCanBuildFrom[A, B]

  def empty[A, B]: ListMap[A, B] = EmptyListMap.asInstanceOf[ListMap[A, B]]

  @SerialVersionUID(-8256686706655863282L)
  private object EmptyListMap extends ListMap[Any, Nothing]
}

/**
  * This class implements immutable maps using a list-based data structure. List map iterators and
  * traversal methods visit key-value pairs in the order whey were first inserted.
  *
  * Entries are stored internally in reversed insertion order, which means the newest key is at the
  * head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and `init`
  * are O(1). Other operations, such as inserting or removing entries, are also O(n), which makes
  * this collection suitable only for a small number of elements.
  *
  * Instances of `ListMap` represent empty maps; they can be either created by calling the
  * constructor directly, or by applying the function `ListMap.empty`.
  *
  * @tparam A the type of the keys contained in this list map
  * @tparam B the type of the values associated with the keys
  *
  * @author Matthias Zenger
  * @author Martin Odersky
  * @version 2.0, 01/01/2007
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
@SerialVersionUID(301002838095710379L)
sealed class ListMap[A, +B] extends AbstractMap[A, B]
  with Map[A, B]
  with MapLike[A, B, ListMap[A, B]]
  with Serializable {

  override def empty = ListMap.empty

  override def size: Int = 0
  override def isEmpty: Boolean = true

  def get(key: A): Option[B] = None

  override def updated[B1 >: B](key: A, value: B1): ListMap[A, B1] = new Node[B1](key, value)

  def +[B1 >: B](kv: (A, B1)): ListMap[A, B1] = new Node[B1](kv._1, kv._2)
  def -(key: A): ListMap[A, B] = this

  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): ListMap[A, B1] =
    if (xs.isEmpty) this
    else ((repr: ListMap[A, B1]) /: xs) (_ + _)

  def iterator: Iterator[(A, B)] = {
    def reverseList = {
      var curr: ListMap[A, B] = this
      var res: List[(A, B)] = Nil
      while (!curr.isEmpty) {
        res = (curr.key, curr.value) :: res
        curr = curr.next
      }
      res
    }
    reverseList.iterator
  }
  private[immutable] def dropLast(n :Int) = this

  protected def key: A = throw new NoSuchElementException("key of empty map")
  protected def value: B = throw new NoSuchElementException("value of empty map")
  protected def next: ListMap[A, B] = throw new NoSuchElementException("next of empty map")

  override def stringPrefix = "ListMap"

  /**
    * essentially the same as += but does not check to see of the key is already in the map
    * so is not safe to call if that cannot be guaranteed
    * @return a new ListMap with the key/value at the start
    */
  private[immutable] def addNoCheck[B1 >: B](key: A, value:B1) : ListMap[A,B1] = new this.Node(key, value)

  /**
    * Represents an entry in the `ListMap`.
    */
  @SerialVersionUID(-6453056603889598734L)
  protected class Node[B1 >: B](override protected val key: A,
                                override protected val value: B1) extends ListMap[A, B1] with Serializable {

    override def size: Int = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(cur: ListMap[A, B1], acc: Int): Int =
      if (cur.isEmpty) acc
      else sizeInternal(cur.next, acc + 1)

    override def isEmpty: Boolean = false

    override def apply(k: A): B1 = {
      val node = findInternal(this,k)
      if (node.isEmpty) throw new NoSuchElementException("key not found: " + k)
      else node.value
    }

    override def get(k: A): Option[B1] = {
      val node = findInternal(this,k)
      if (node.isEmpty) None
      else Some(node.value)
    }

    override def contains(k: A): Boolean = !findInternal(this, k).isEmpty

    @tailrec private[this] def findInternal(cur: ListMap[A, B1], k: A): ListMap[A,B1] =
      if (cur.isEmpty || k == cur.key) cur
      else findInternal(cur.next, k)


    override def updated[B2 >: B1](k: A, v: B2): ListMap[A, B2] = {
      val node = findInternal(this,k)
      if (node.isEmpty) new Node(k,v)
      else if (node.value == v) this
      else {
        val m = this - k
        new m.Node[B2](k, v)
      }
    }

    override def +[B2 >: B1](kv: (A, B2)): ListMap[A, B2] = updated(kv._1, kv._2)

    override def -(k: A): ListMap[A, B1] = removeInternal(k, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: ListMap[A, B1], acc: List[ListMap[A, B1]]): ListMap[A, B1] =
      if (cur.isEmpty) acc.last
      else if (k == cur.key) (cur.next /: acc) { case (t, h) => new t.Node(h.key, h.value) }
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListMap[A, B1] = ListMap.this

    override def last: (A, B1) = (key, value)
    override def init: ListMap[A, B1] = next

    private[immutable] override def dropLast(n :Int): ListMap[A, B1] = {
      dropLastInternal(this, n)
    }
    @tailrec private def dropLastInternal(cur:ListMap[A,B1], n :Int): ListMap[A, B1] = {
      if(cur.isEmpty || n <= 0) cur
      else dropLastInternal(cur.init, n-1)
    }

  }
}
