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

package scala.reflect.internal
import java.util.Collections
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.reflect.internal.util.cache._

@Threads(4)
@BenchmarkMode(Array(org.openjdk.jmh.annotations.Mode.AverageTime))
class NameLookupBenchmark4Threads extends NameLookupBenchmark
@Threads(2)
@BenchmarkMode(Array(org.openjdk.jmh.annotations.Mode.AverageTime))
class NameLookupBenchmark2Threads extends NameLookupBenchmark

@BenchmarkMode(Array(org.openjdk.jmh.annotations.Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Threads(1)
@Warmup(iterations = 3)
@Measurement(iterations = 10)
@State(Scope.Benchmark)
class NameLookupBenchmark {
  var map : ConcurrentHashMap[String, TestNode] = _
  var cache : NodeInterner[TestNode]= _
  var cacheJ : NodeJInterner[TestNodeJ]= _
  var data : Array[String]= _
  @Setup(Level.Trial) def setup(): Unit = {
    map = new ConcurrentHashMap[String, TestNode]()
    cache = new CacheImpl
    cacheJ = new CacheImplJ
    data = Array.tabulate[String](10000)(_.toString)
    Collections.shuffle(java.util.Arrays.asList(data))

    //init
    chmGet(new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous."))
    lookup(new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous."))

    cache.initHack

  }

  @Benchmark
  def chmGet(bh: Blackhole): Unit = {
    var i = 0
    val d = data
    while (i < d.length) {
      val key = data(i)
      var found = map.get(key)
      if (found eq null) {
        val x = new TestNode(key,null)
        found = map.putIfAbsent(key,x)
        if (found eq null) found = x
      }
      bh.consume(found)
      i += 1
    }
  }
//  @Benchmark
//  def chmGetExisting(bh: Blackhole): Unit = {
//    var i = 0
//    val d = data
//    while (i < d.length) {
//      val key = data(i)
//      var found = map.get(key)
//      if (found eq null) {
//        val x = new TestNode(key,null)
//        found = map.put(key,x)
//        if (found eq null) found = x
//      }
//      bh.consume(found)
//      i += 1
//    }
//  }

//  @Benchmark
//  def chmCompute(bh: Blackhole): Unit = {
//    var i = 0
//    val d = data
//    while (i < d.length) {
//      val key = data(i)
//      val found = map.computeIfAbsent(key, new TestNode(_, null))
//      bh.consume(found)
//      i += 1
//    }
//  }

  @Benchmark
  def lookup(bh: Blackhole): Unit = {
    var i = 0
    val d = data
    while (i < d.length) {
      val key = data(i)
      val found = cache.insertOrFind(key)
      bh.consume(found)
      i += 1
    }
  }
  @Benchmark
  def lookup2(bh: Blackhole): Unit = {
    var i = 0
    val d = data
    while (i < d.length) {
      val key = data(i)
      val found = cache.insertOrFind2(key)
      bh.consume(found)
      i += 1
    }
  }
  @Benchmark
  def lookup3(bh: Blackhole): Unit = {
    var i = 0
    val d = data
    while (i < d.length) {
      val key = data(i)
      val found = cache.insertOrFind3(key)
      bh.consume(found)
      i += 1
    }
  }
  @Benchmark
  def lookup4(bh: Blackhole): Unit = {
    var i = 0
    val d = data
    while (i < d.length) {
      val key = data(i)
      val found = cache.insertOrFind4(key)
      bh.consume(found)
      i += 1
    }
  }
//  @Benchmark
//  def lookupJ(bh: Blackhole): Unit = {
//    var i = 0
//    val d = data
//    while (i < d.length) {
//      val key = data(i)
//      val found = cacheJ.insertOrFind(key)
//      bh.consume(found)
//      i += 1
//    }
//  }
//  @Benchmark
//  def lookupExisting(bh: Blackhole): Unit = {
//    var i = 0
//    val d = data
//    while (i < d.length) {
//      val key = data(i)
//      val found = cache.insertOrFind(key)
//      bh.consume(found)
//      i += 1
//    }
//  }
}
class CacheImpl extends NodeInterner[TestNode]{

  override def createNode(key: String, next: TestNode)= new TestNode(key,next){}
}
class TestNode(key: String, next: Node) extends Node(key,next)

class CacheImplJ extends NodeJInterner[TestNodeJ]{

  override protected def createNodeJ(key: String, next: NodeJ) = new TestNodeJ(key,next){}
}
class TestNodeJ(key: String, next: NodeJ) extends NodeJ(key,next)

