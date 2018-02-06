package scala.collection

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole


@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class DistinctBenchmark {

  @Param(Array(
    "distinct",
    "distinctTrivial",
    "distinctSimple",
    "distinctSimpleJavaHashSet",
    "distinctSimpleWhileJavaHashSet",
    "distinctSimpleWhileIteratorOneAdd",
    "distinctSimpleWhileJavaHashSetIterator",
    "distinctSimpleWhile",
    "distinct1",
    "distinctWhile",
    "distinct2",
    "distinct2While",
    "distinctSmall",
    "distinctHelper"
  ))
  var fnName:String = _

  @Param(Array("0","1","2", "5", "10", "20", "50", "100", "1000"))
  var a1_sizeString:String = _


  var size: Int = _

  var underTestDistinct : Seq[String] = null
  var underTestLast : Seq[String] = null
  var underTestFirst : Seq[String] = null
  var underTestDup1 : Seq[String] = null
  var underTestDup2 : Seq[String] = null

  @Setup(Level.Trial) def init(): Unit = {
    size = a1_sizeString.toInt
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

    doFn = fnName match {
      case "distinct" => (a: Seq[String]) => a.distinct
      case "distinctTrivial" => (a: Seq[String]) => a.distinctTrivial
      case "distinctSimple" => (a: Seq[String]) => a.distinctSimple
      case "distinctSimpleJavaHashSet" => (a: Seq[String]) => a.distinctSimpleJavaHashSet
      case "distinctSimpleWhileJavaHashSet" => (a: Seq[String]) => a.distinctSimpleWhileJavaHashSet
      case "distinctSimpleWhileIteratorOneAdd" => (a: Seq[String]) => a.distinctSimpleWhileIteratorOneAdd
      case "distinctSimpleWhileJavaHashSetIterator" => (a: Seq[String]) => a.distinctSimpleWhileJavaHashSetIterator
      case "distinctSimpleWhile" => (a: Seq[String]) => a.distinctSimpleWhile
      case "distinct1" => (a: Seq[String]) => a.distinct1
      case "distinctWhile" => (a: Seq[String]) => a.distinctWhile
      case "distinct2" => (a: Seq[String]) => a.distinct2
      case "distinct2While" => (a: Seq[String]) => a.distinct2While
      case "distinctSmall" => (a: Seq[String]) => a.distinctSmall
      case "distinctHelper" => (a: Seq[String]) => a.distinctHelper
    }

  }
  var doFn:  Seq[String] =>  Seq[String] = _

  @Benchmark def testDistinct(bh: Blackhole): Unit = {
    bh.consume(doFn(underTestDistinct))
  }

  @Benchmark def testLast(bh: Blackhole): Unit = {
    bh.consume(doFn(underTestLast))
  }

  @Benchmark def testFirst(bh: Blackhole): Unit = {
    bh.consume(doFn(underTestFirst))
  }

  @Benchmark def testDup1(bh: Blackhole): Unit = {
    bh.consume(doFn(underTestDup1))
  }

  @Benchmark def testDup2(bh: Blackhole): Unit = {
    bh.consume(doFn(underTestDup2))
  }

}
