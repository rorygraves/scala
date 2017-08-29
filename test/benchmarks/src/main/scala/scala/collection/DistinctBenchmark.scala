package scala.collection

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole


@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class DistinctBenchmark {
  @Param(Array("0","1","2", "5", "9", "10", "11", "12", "100"))
  var sizeString:String = _
  var size: Int = _

  var underTestDistinct : Seq[String] = null
  var underTestLast : Seq[String] = null
  var underTestFirst : Seq[String] = null
  var underTestDup1 : Seq[String] = null
  var underTestDup2 : Seq[String] = null

  @Setup(Level.Trial) def init(): Unit = {
    size = sizeString.toInt
    val b1 = List.newBuilder[String]
    val b2 = List.newBuilder[String]
    0 until size foreach {
      i =>
        b1 += i.toString
        b2 += i.toString
        b2 += i.toString
    }

    underTestDistinct = b1.result()
    if (size > 0) {
      underTestFirst = underTestDistinct.tail
      underTestLast = underTestDistinct.tail
      underTestFirst = underTestDistinct.head +: underTestDistinct
      underTestLast = underTestDistinct :+ underTestDistinct.head
    }
    underTestDup1 = b2.result()
    underTestDup2 = underTestDistinct ++ underTestDistinct

  }

  @Benchmark def testDistinct(bh: Blackhole): Unit = {
    bh.consume(underTestDistinct.distinct)
  }

  @Benchmark def testLast(bh: Blackhole): Unit = {
    bh.consume(underTestLast.distinct)
  }

  @Benchmark def testFirst(bh: Blackhole): Unit = {
    bh.consume(underTestFirst.distinct)
  }

  @Benchmark def testDup1(bh: Blackhole): Unit = {
    bh.consume(underTestDup1.distinct)
  }

  @Benchmark def testDup2(bh: Blackhole): Unit = {
    bh.consume(underTestDup2.distinct)
  }

}
