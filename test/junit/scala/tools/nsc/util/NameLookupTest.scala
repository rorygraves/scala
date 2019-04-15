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

package scala.tools.nsc.util

import java.util
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap

import org.junit._
import Assert._

import scala.reflect.internal.util.cache.{Node, NodeInterner, NodeJ, NodeJInterner}
import scala.util.Random

object foo extends App
class NameLookupTest {
  val cache : NodeInterner[TestNode] = new CacheImpl
  val cacheJ : NodeJInterner[TestNodeJ] = new CacheImplJ
  val data : Array[String]= Array.tabulate[String](10000)(_.toString)
  Collections.shuffle(java.util.Arrays.asList(data))

  @Test def size(): Unit = {
    assertEquals(0, cache.size)
    val f1 = cache.insertOrFind("foo")
    assertEquals(1, cache.size)
    val f2 = cache.insertOrFind("foo")
    assertSame(f1,f2)
    assertEquals(1, cache.size)
    val f3 = cache.insertOrFind(new String("foo"))
    assertSame(f1,f3)
  }

  @Test def insertAll(): Unit = {
    import scala.collection.JavaConverters._
    val all = new util.IdentityHashMap[TestNode, String]()
    val random = new Random()
    for (i <- 1 to data.length * 10) {
      val r = random.nextInt(data.length)
      val key = data(r)
      val cached = cache.insertOrFind(key)
      val existing = all.put(cached, key)
      assertTrue((existing eq null) || existing == key)
    }
    for (i <- 0 until data.length) {
      val key = data(i)
      val existing = all.put(cache.insertOrFind(key), key)
      assertTrue((existing eq null) || existing == key)
    }
    val bad = all.asScala groupBy {_._2} filter {_._2.size > 1}
    assertEquals(0, bad.size)
    assertEquals(data.length, cache.size)
  }
  @Test def sizeJ(): Unit = {
    assertEquals(0, cacheJ.size)
    val f1 = cacheJ.insertOrFind("foo")
    assertEquals(1, cacheJ.size)
    val f2 = cacheJ.insertOrFind("foo")
    assertSame(f1,f2)
    assertEquals(1, cacheJ.size)
    val f3 = cacheJ.insertOrFind(new String("foo"))
    assertSame(f1,f3)
  }

  @Test def insertAllJ(): Unit = {
    import scala.collection.JavaConverters._
    val all = new util.IdentityHashMap[TestNodeJ, String]()
    val random = new Random()
    for (i <- 1 to data.length * 10) {
      val r = random.nextInt(data.length)
      val key = data(r)
      val cacheJd = cacheJ.insertOrFind(key)
      val existing = all.put(cacheJd, key)
      assertTrue((existing eq null) || existing == key)
    }
    for (i <- 0 until data.length) {
      val key = data(i)
      val existing = all.put(cacheJ.insertOrFind(key), key)
      assertTrue((existing eq null) || existing == key)
    }
    val bad = all.asScala groupBy {_._2} filter {_._2.size > 1}
    assertEquals(0, bad.size)
    assertEquals(data.length, cacheJ.size)
  }

}


class CacheImpl extends NodeInterner[TestNode]{
  val created = new util.HashSet[String]()
  override def createNode(key: String, next: TestNode): TestNode = {
    assert (created.add(key), key)
    new TestNode(key,next){}
  }
}
class TestNode(key: String, next: Node) extends Node(key,next)

class CacheImplJ extends NodeJInterner[TestNodeJ]{
  val created = new util.HashSet[String]()

  override protected def createNodeJ(key: String, node: NodeJ): TestNodeJ = {
    assert (created.add(key), key)
    new TestNodeJ(key,node){}
  }
}
class TestNodeJ(key: String, next: NodeJ) extends NodeJ(key,next)