package scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.{Any, AnyRef, Array, Int, Long, Unit}
import scala.Predef.intWrapper
import scala.Predef.genericArrayOps

/**
  * This test Array performance in same way with collection-strawman
  * https://github.com/scala/collection-strawman/blob/master/benchmarks/time/src/main/scala/strawman/collection/immutable/ImmutableArrayBenchmark.scala
  */
@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@State(Scope.Benchmark)
class ArrayBaselineBenchmark {

  @Param(scala.Array("0", "1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "4096", "131070", "7312102"))
  var size: Int = _

  var xs: Array[Long] = _
  var zs: Array[Long] = _
  var zipped: Array[(Long, Long)] = _
  var randomIndices: scala.Array[Int] = _
  def fresh(n: Int) = Array((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initTrial(): Unit = {
    xs = fresh(size)
    zs = fresh((size / 1000) max 2).map(-_)
    zipped = xs.map(x => (x, x))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
    }
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def access_slice(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      bh.consume(xs.slice(size - size / (i + 1), size))
      i += 1
    }
  }
}
