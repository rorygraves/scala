package scala.reflect

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.reflect.internal.names._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
abstract class AbstractNamesBenchmark {
  var names: Array[String] = null
  var namesChars: Array[Array[Char]] = null

  @Param(Array("0", "10", "10000", "100000", "1000000"))
  var arraySize: Int = _

  def testType: String

  var fullNameTable: NameTable[_ <: AnyRef] = _
  var newNameTable: () => NameTable[_ <: AnyRef] = _

  @Setup def setup(): Unit = {
    names = Array.tabulate(arraySize)(i => s"name$i")
    namesChars = names map (_.toArray)
    newNameTable = testType match {
      case "Existing" => () => new ExistingNameTable
      case "ExistingSynchronized" => () => new ExistingSynchronizedNameTable
      case "MapNameTable" => () => new MapNameTable(new NameBase(_))
      case "ConcurrentMapNameTable1" => () => new ConcurrentMapNameTable1(new NameBase(_))
      case "ConcurrentMapNameTable2" => () => new ConcurrentMapNameTable2(new NameBase(_))
      case "ConcurrentMapNameTable3" => () => new ConcurrentMapNameTable3(new NameBase(_))
      case "WeakAutoTrimConcurrentMapNameTable1" => () => new WeakAutoTrimConcurrentMapNameTable1[AnyRef](new NameBase(_))
      case "WeakAutoTrimConcurrentMapNameTable2" => () => new WeakAutoTrimConcurrentMapNameTable1[AnyRef](new NameBase(_))
      case "WeakNoAutoTrimConcurrentMapNameTable1" => () => new WeakNoAutoTrimConcurrentMapNameTable1[AnyRef](new NameBase(_))
      case "WeakNoAutoTrimConcurrentMapNameTable2" => () => new WeakNoAutoTrimConcurrentMapNameTable2[AnyRef](new NameBase(_))
      case "StrongConcurrentNodeInterner" => () => new StrongConcurrentNodeInterner(new NameBase(_))
      case "WeakFixedSizeNoAutoTrimConcurrentNodeInterner" => () => new WeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_))
      case "HashDefault" => () => new WeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_))
      case "Hash" => () => new WeakFixedSizeNoAutoTrimConcurrentNodeInternerHash(new NameBase(_))
      case "NoHash" => () => new WeakFixedSizeNoAutoTrimConcurrentNodeInternerNoHash(new NameBase(_))
      case "WeakFixedSizeAutoTrimConcurrentNodeInterner" => () => new WeakFixedSizeAutoTrimConcurrentNodeInterner(new NameBase(_))
      case "Find0" => () =>
        new WeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_)) {
          override def find(chars: Array[Char], start: Int, count: Int) = super.find(chars, start, count)
        }
      case "Find1" => () =>
        new WeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_)) {
          override def find(chars: Array[Char], start: Int, count: Int) = super.find1(chars, start, count)
        }
      case "Find2" => () =>
        new WeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_)) {
          override def find(chars: Array[Char], start: Int, count: Int) = super.find2(chars, start, count)
        }
      case "Find3" => () =>
        new WeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_)) {
          override def find(chars: Array[Char], start: Int, count: Int) = super.find3(chars, start, count)
        }
      case "Safe" => () =>
        new WeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_)) {
          override def find(chars: Array[Char], start: Int, count: Int) = super.find(chars, start, count)
        }
      case "Unsafe" => () =>
        new UnsafeWeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_)) {
          override def find(chars: Array[Char], start: Int, count: Int) = super.find(chars, start, count)
        }
      case "Tail" => () =>
        new WeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_)) {
          override def find(key: String) = super.find(key)
        }
      case "NoTail" => () =>
        new UnsafeWeakFixedSizeNoAutoTrimConcurrentNodeInterner(new NameBase(_)) {
          override def find(key: String) = super.findNoTail(key)
        }
    }
    fullNameTable = newNameTable()
    names foreach {
      fullNameTable.find(_)
    }
  }

  @Benchmark def findOldString(bh: Blackhole): Any = {
    val nameTable: NameTable[_ <: AnyRef] = fullNameTable
    val res = new Array[AnyRef](arraySize)
    var i = 0
    while (i < arraySize) {
      res(i) = nameTable.find(names(i))
      i += 1
    }
    bh.consume(res)
  }

  @Benchmark def findOldChars(bh: Blackhole): Any = {
    val nameTable: NameTable[_ <: AnyRef] = fullNameTable
    var i = 0
    val res = new Array[AnyRef](arraySize)
    while (i < arraySize) {
      val c = namesChars(i)
      res(i) = nameTable.find(c, 0, c.length)
      i += 1
    }
    bh.consume(res)
  }
}

@Threads(1)
class NamesBenchmark extends AbstractNamesBenchmark {
  @Param(Array("Existing", "ExistingSynchronized", "MapNameTable",
    "ConcurrentMapNameTable1", "ConcurrentMapNameTable2", "ConcurrentMapNameTable3",
    "WeakAutoTrimConcurrentMapNameTable1", "WeakAutoTrimConcurrentMapNameTable2",
    "WeakNoAutoTrimConcurrentMapNameTable1", "WeakNoAutoTrimConcurrentMapNameTable2",
    "StrongConcurrentNodeInterner", "WeakFixedSizeNoAutoTrimConcurrentNodeInterner",
    "WeakFixedSizeAutoTrimConcurrentNodeInterner",
    "HashDefault", "Hash", "NoHash",
    "Find0", "Find1", "Find2", "Find3", "Safe", "Unsafe", "Tail", "NoTail"))
  var testImpl: String = _

  override def testType = testImpl

  @Benchmark def findNewString(bh: Blackhole): Any = {
    val nameTable = newNameTable()
    val res = new Array[AnyRef](arraySize)
    var i = 0
    while (i < arraySize) {
      res(i) = nameTable.find(names(i))
      i += 1
    }
    bh.consume(res)
  }

  @Benchmark def findNewChars(bh: Blackhole): Any = {
    val nameTable = newNameTable()
    var i = 0
    val res = new Array[AnyRef](arraySize)
    while (i < arraySize) {
      val c = namesChars(i)
      res(i) = nameTable.find(c, 0, c.length)
      i += 1
    }
    bh.consume(res)
  }
}

@Threads(4)
class NamesBenchmarkMT extends AbstractNamesBenchmark {
  @Param(Array("ExistingSynchronized",
    "ConcurrentMapNameTable1", "ConcurrentMapNameTable2", "ConcurrentMapNameTable3",
    "WeakAutoTrimConcurrentMapNameTable1", "WeakAutoTrimConcurrentMapNameTable2",
    "WeakNoAutoTrimConcurrentMapNameTable1", "WeakNoAutoTrimConcurrentMapNameTable2",
    "StrongConcurrentNodeInterner", "WeakFixedSizeNoAutoTrimConcurrentNodeInterner",
    "WeakFixedSizeAutoTrimConcurrentNodeInterner", "Find0", "Find1", "Find2", "Find3", "Safe", "Unsafe", "Tail", "NoTail"))
  var testImpl: String = _

  override def testType = testImpl
}