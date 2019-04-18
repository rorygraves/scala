/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.internal.util.cache

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReferenceArray}
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

abstract class Node(val key: String, @volatile private[cache] var next: Node) extends AnyRef

abstract class NodeInterner[T <: Node] {

  final def size = approxSize.get
  protected def createNode(key: String, node: T): T

  final def insertOrFind(key: String): T = {
    val hash = key.hashCode
    val improved = hash ^ (hash >>> 16)
    var oldTail: Node = null

    var node: Node = null
    do {
      //deliberately hiding this.data
      val data = initial()
      val bucket = improved & (data.length - 1)
      val head = data.get(bucket)
      node = head
      while ((node ne null) &&
        // we have already checked the tail
        (node ne oldTail) &&
        // its not equal. HashCode is cheap for strings and a good discriminant
        (node.key.hashCode() != hash || node.key != key))
        node = node.next
      if (node eq oldTail) node = null
      if (node eq null) {
        // minor optimisation - we can skip this tail if we have to retry
        // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
        oldTail = head
        val newNode = createNode(key, head.asInstanceOf[T])
        if (data.compareAndSet(bucket, head, newNode) &&
          // volatile read to ensure that we have not grown in another thread
          //must be after the CAS and guard afterInsert
          (data eq this.data.get)) {
          afterInsert(data)
          node = newNode
        }
      } else if (
      // volatile read to ensure that we have not grown in another thread
        data ne this.data.get) {
        node = null
      }
    } while (node eq null)
    node.asInstanceOf[T]
  }
  final def contains(key: String): Boolean = {
    getExistingImpl(key) ne null
  }
  final def get(key: String): Option[T] = {
    Option(getExistingImpl(key).asInstanceOf[T])
  }
  final def getExistingImpl(key: String): Node = {
    val data = initial
    val hash = key.hashCode
    val improved = hash ^ (hash >>> 11) ^ (hash >>> 22)
    val list = data.get(improved & (data.length() -1))
      getExistingImpl(list,key)
  }
  @tailrec private def getExistingImpl(list: Node, key: String): Node = {
    if (list eq null) null
    else if (list.key == key) list
    else getExistingImpl(list.next, key)
  }

  /**
    * get the root of data
    *
    * @return
    */
  private def initial(): AtomicReferenceArray[Node] = {
    //volatile read
    var result = data.get()
    //null indicates it is in the process of being rehashed
    //updates are applied with synchronisation lock on data
    if (result eq null) data.synchronized {
      //when we have the lock we can guarantee that the other threads rehash is complete
      result = data.get()
      assert(result ne null)
    }
    result
  }

  /**
    * rehash and grow
    */
  private def afterInsert(data: AtomicReferenceArray[Node]): Unit = {
    val newSize = approxSize.incrementAndGet()
    val origLength = data.length
    if (origLength < newSize) {
      this.data.synchronized {
        // if the value has changed already then its not our problem
        if (this.data.compareAndSet(data, null)) {
          val size = origLength * 2
          val mask = origLength
          val newData = new AtomicReferenceArray[Node](size)

          var head0: Node = null
          var head1: Node = null
          var sourceIdx = 0
          while (sourceIdx < origLength) {
            head0 = null
            head1 = null
            var tail0: Node = null
            var tail1: Node = null
            var sourceNode = data.get(sourceIdx)
            while (sourceNode ne null) {
              val hash = sourceNode.key.hashCode
              val improved = hash ^ (hash >>> 11) ^ (hash >>> 22)
              if ((improved & mask) == 0) {
                if (head0 eq null) head0 = sourceNode
                else tail0.next = sourceNode
                tail0 = sourceNode
              } else {
                if (head1 eq null) head1 = sourceNode
                else tail1.next = sourceNode
                tail1 = sourceNode
              }
              sourceNode = sourceNode.next
            }
            if (tail0 ne null) tail0.next = null
            if (tail1 ne null) tail1.next = null
            newData.set(sourceIdx, head0)
            newData.set(sourceIdx + mask, head1)
            sourceIdx += 1
          }
          this.data.set(newData)
        }
      }
    }
  }

  private[this] val approxSize = new AtomicInteger
  private[this] final val data = new AtomicReference(new AtomicReferenceArray[Node](1 << 16))

  //hacks
  def initHack: Unit = {
    data2 = data.get
    data2NotG = data2
    data3 = new Array[Node](data2.length)
    for ( i <- 0 until data2.length) {
      data3(i) = data2.get(i)
    }
    data3NotG = data3
    System.arraycopy(data3, 0, data3NotG2,0, data3.length)
  }


  @volatile private[this] final var data2: AtomicReferenceArray[Node] = _
  private[this] final var data2NotG: AtomicReferenceArray[Node] = _
  @volatile private[this] final var data3: Array[Node] = _
  private[this] final var data3NotG: Array[Node] = _
  private[this] final val data3NotG2: Array[Node] = new Array[Node](1 << 16)


  private def initial2(): AtomicReferenceArray[Node] = {
    //volatile read
    var result = data2
    //null indicates it is in the process of being rehashed
    //updates are applied with synchronisation lock on data
    if (result eq null) data.synchronized {
      //when we have the lock we can guarantee that the other threads rehash is complete
      result = data2
      assert(result ne null)
    }
    result
  }
  private def initial3(): Array[Node] = {
    //volatile read
    var result = data3
    //null indicates it is in the process of being rehashed
    //updates are applied with synchronisation lock on data
    if (result eq null) data.synchronized {
      //when we have the lock we can guarantee that the other threads rehash is complete
      result = data3
      assert(result ne null)
    }
    result
  }
  final def insertOrFind2(key: String): T = {
    val hash = key.hashCode
    val improved = hash ^ (hash >>> 16)
    var oldTail: Node = null

    var node: Node = null
    do {
      //deliberately hiding this.data
      val data = initial2
      val bucket = improved & (data.length - 1)
      val head = data.get(bucket)
      node = head
      while ((node ne null) &&
        // we have already checked the tail
        (node ne oldTail) &&
        // its not equal. HashCode is cheap for strings and a good discriminant
        (node.key.hashCode() != hash || node.key != key))
        node = node.next
      if (node eq oldTail) node = null
      if (node eq null) {
        // minor optimisation - we can skip this tail if we have to retry
        // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
        oldTail = head
        val newNode = createNode(key, head.asInstanceOf[T])
        if (data.compareAndSet(bucket, head, newNode) &&
          // volatile read to ensure that we have not grown in another thread
          //must be after the CAS and guard afterInsert
          (data eq this.data2)) {
          afterInsert(data)
          node = newNode
        }
      } else if (
      // volatile read to ensure that we have not grown in another thread
        data ne this.data2) {
        node = null
      }
    } while (node eq null)
    node.asInstanceOf[T]
  }
  final def insertOrFind2NotGrowing(key: String): T = {
    val hash = key.hashCode
    val improved = hash ^ (hash >>> 16)
    //    var oldTail: Node = null

    var node: Node = null
    val data = data2NotG
    val bucket = improved & (data.length - 1)
    do {
      //deliberately hiding this.data
      val head = data.get(bucket)
      node = head
      while ((node ne null) &&
        //        // we have already checked the tail
        //        (node ne oldTail) &&
        // its not equal. HashCode is cheap for strings and a good discriminant
        (node.key.hashCode() != hash || node.key != key))
        node = node.next
      //  if (node eq oldTail) node = null
      if (node eq null) {
        //        // minor optimisation - we can skip this tail if we have to retry
        //        // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
        //        oldTail = head
        val newNode = createNode(key, head.asInstanceOf[T])
        if (data.compareAndSet(bucket, head, newNode)){
          approxSize.incrementAndGet()
          node = newNode
        }
      }
    } while (node eq null)
    node.asInstanceOf[T]
  }
  final def insertOrFind2NotGrowing2(key: String): T = {
    val hash = key.hashCode
    val improved = hash ^ (hash >>> 16)
        var oldTail: Node = null

    var node: Node = null
    val data = data2NotG
    val bucket = improved & (data.length - 1)
    do {
      //deliberately hiding this.data
      val head = data.get(bucket)
      node = head
      while ((node ne null) &&
                // we have already checked the tail
                (node ne oldTail) &&
        // its not equal. HashCode is cheap for strings and a good discriminant
        (node.key.hashCode() != hash || node.key != key))
        node = node.next
        if (node eq oldTail) node = null
      if (node eq null) {
        val newNode = createNode(key, head.asInstanceOf[T])
        if (data.compareAndSet(bucket, head, newNode)){
          approxSize.incrementAndGet()
          node = newNode
        } else {
          // minor optimisation - we can skip this tail if we have to retry
          // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
          oldTail = head
        }
      }
    } while (node eq null)
    node.asInstanceOf[T]
  }
  final def insertOrFind3(key: String): T = {
    val hash = key.hashCode
    val improved = hash ^ (hash >>> 16)
    var oldTail: Node = null

    var node: Node = null
    do {
      //deliberately hiding this.data
      val data = initial3
      val bucket = improved & (data.length - 1)
      val head = data(bucket)
      node = head
      while ((node ne null) &&
        // we have already checked the tail
        (node ne oldTail) &&
        // its not equal. HashCode is cheap for strings and a good discriminant
        (node.key.hashCode() != hash || node.key != key))
        node = node.next
      if (node eq oldTail) node = null
      if (node eq null) {
        // minor optimisation - we can skip this tail if we have to retry
        // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
        oldTail = head
        val newNode = createNode(key, head.asInstanceOf[T])
        if (true && //data(bucket, head, newNode) &&
          // volatile read to ensure that we have not grown in another thread
          //must be after the CAS and guard afterInsert
          (data eq this.data3)) {
          data(bucket) = newNode
          //afterInsert(data)
          node = newNode
        }
      } else if (
      // volatile read to ensure that we have not grown in another thread
        data ne this.data3) {
        node = null
      }
    } while (node eq null)
    node.asInstanceOf[T]
  }
  final def insertOrFind3NG1(key: String): T = {
    val hash = key.hashCode
    val improved = hash ^ (hash >>> 16)
    var oldTail: Node = null

    var node: Node = null
    //deliberately hiding this.data
    val data = data3NotG
    val bucket = improved & (data.length - 1)
    do {
      val head = data(bucket)
      node = head
      while ((node ne null) &&
        // we have already checked the tail
        (node ne oldTail) &&
        // its not equal. HashCode is cheap for strings and a good discriminant
        (node.key.hashCode() != hash || node.key != key))
        node = node.next
      if (node eq oldTail) node = null
      if (node eq null) {
        // minor optimisation - we can skip this tail if we have to retry
        // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
        oldTail = head
        val newNode = createNode(key, head.asInstanceOf[T])
        if (true //data(bucket, head, newNode) &&
        // volatile read to ensure that we have not grown in another thread
        //must be after the CAS and guard afterInsert
        //(data eq this.data3))
        ){
          data(bucket) = newNode
          //afterInsert(data)
          node = newNode
        }
      }
    } while (node eq null)
    node.asInstanceOf[T]
  }
  final def insertOrFind3NG2(key: String): T = {
    val hash = key.hashCode
    val improved = hash ^ (hash >>> 16)
    var oldTail: Node = null

    var node: Node = null
    //deliberately hiding this.data
    val data = data3NotG
    val bucket = improved & (data.length - 1)
    do {
      val head = data(bucket)
      node = head
      while ((node ne null) &&
        // we have already checked the tail
        (node ne oldTail) &&
        // its not equal. HashCode is cheap for strings and a good discriminant
        (node.key.hashCode() != hash || node.key != key))
        node = node.next
      if (node eq oldTail) node = null
      if (node eq null) {
        // minor optimisation - we can skip this tail if we have to retry
        // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
        oldTail = head
        val newNode = createNode(key, head.asInstanceOf[T])
        if (true //data(bucket, head, newNode) &&
        // volatile read to ensure that we have not grown in another thread
        //must be after the CAS and guard afterInsert
        //(data eq this.data3))
        ){
          data(bucket) = newNode
          //afterInsert(data)
          node = newNode
        }
      }
    } while (node eq null)
    node.asInstanceOf[T]
  }
}

