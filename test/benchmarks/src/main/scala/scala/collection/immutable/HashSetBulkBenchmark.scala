package scala.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
abstract class HashSetBulkBenchmark {
  @Param(Array(
    "10",
    "100",
    "1000",
    "10000",
    "100000"
  ))
  var size: Int = _

  @Param(Array("0", "20", "40", "60", "80", "90", "100"))
  var sharing: Int = _

  var baseData: Array[HashSet[String]] = _
  var overlap: Array[HashSet[String]] = _
  var overlap2: Array[HashSet[String]] = _
  var shared: Array[HashSet[String]] = _

  @Setup(Level.Trial) def initKeys(): Unit = {

    def generate(prefix: String, size: Int) = {
      Array.tabulate(30)(i => (0 until size).map(k =>
        s"key $i $k")(scala.collection.breakOut): HashSet[String])
    }

    baseData = generate("", size)

    overlap = new Array[HashSet[String]](baseData.length - 10)
    overlap2 = new Array[HashSet[String]](baseData.length - 10)
    for (i <- 0 until baseData.length - 10) {
      var s1: HashSet[String] = HashSet.empty[String]
      var s2: HashSet[String] = HashSet.empty[String]
      for (j <- i until i + 10) {
        baseData(j) foreach {
          x =>
            s1 += x
            s2 += x
        }
      }
      overlap(i) = s1
      overlap2(i) = s2

    }

    shared = new Array[HashSet[String]](overlap.size)
    shared(0) = overlap(0)
    for (i <- 1 until baseData.length - 10) {
      shared(i) = shared(i - 1) -- baseData(i - 1) ++ baseData(i + 10)
    }
  }

  @OperationsPerInvocation(10)
  @Benchmark def opWithDistinct(bh: Blackhole): Unit = {
    var i = 10;
    while (i < 20) {
      operation(bh, baseData(i - 1), baseData(i))
      i += 1
    }
  }

  @OperationsPerInvocation(10)
  @Benchmark def opWithOverlap(bh: Blackhole): Unit = {
    var i = 10;
    while (i < 20) {
      operation(bh, overlap(i - (10 - sharing / 10)), overlap2(i))
      i += 1
    }
  }

  @OperationsPerInvocation(9)
  @Benchmark def opWithShared(bh: Blackhole): Unit = {
    var i = 11;
    while (i < 20) {
      operation(bh, shared(i - (10 - sharing / 10)), shared(i))
      i += 1
    }
  }

  @OperationsPerInvocation(9)
  @Benchmark def opLargeWithContained(bh: Blackhole): Unit = {
    var i = 11;
    while (i < 20) {
      operation(bh, overlap(i), baseData(i))
      i += 1
    }
  }

  @OperationsPerInvocation(9)
  @Benchmark def opLargeWithEmpty(bh: Blackhole): Unit = {
    var i = 11;
    while (i < 20) {
      operation(bh, shared(i - (10 - sharing / 10)), HashSet.empty)
      i += 1
    }
  }

  @OperationsPerInvocation(9)
  @Benchmark def opContainedWithLarge(bh: Blackhole): Unit = {
    var i = 11;
    while (i < 20) {
      operation(bh,
        baseData(i),
        shared(i - (10 - sharing / 10)))
      i += 1
    }
  }

  @OperationsPerInvocation(10)
  @Benchmark def opEmptyWithData(bh: Blackhole): Unit = {
    var i = 10;
    while (i < 20) {
      operation(bh, HashSet.empty, shared(i - (10 - sharing / 10)))
      i += 1
    }
  }

  def operation(bh: Blackhole, set1: HashSet[String], set2: HashSet[String]):Any
}
class HashSetPlusPlusBenchmark extends HashSetBulkBenchmark {
  def operation(bh: Blackhole, set1: HashSet[String], set2: HashSet[String]) = {
    val r = set1 ++ set2
    bh.consume(r)
    r
  }
  @OperationsPerInvocation(9)
  @Benchmark override def opLargeWithContained(bh: Blackhole): Unit = {
    var i = 10;
    while (i < 20) {
      val p1 = overlap(i)
      val p2 = baseData(i)
      val res = operation(bh, p1, p2)
      //assert (res.size == res.iterator.toArray.size)

      i += 1
    }
  }


}

class HashSetUnionBenchmark extends HashSetBulkBenchmark {
  def operation(bh: Blackhole, set1: HashSet[String], set2: HashSet[String]) = {
    bh.consume(set1.union(set2))
  }
}
class HashSetDiffBenchmark extends HashSetBulkBenchmark {
  def operation(bh: Blackhole, set1: HashSet[String], set2: HashSet[String]) = {
    bh.consume(set1.diff(set2))
  }
}
class HashSetIntersectBenchmark extends HashSetBulkBenchmark {
  def operation(bh: Blackhole, set1: HashSet[String], set2: HashSet[String]) = {
    bh.consume(set1.intersect(set2))
  }
}
//for testing, debugging etc
object Test extends App {

  val bh = new Blackhole("Today's password is swordfish. I understand instantiating Blackholes directly is dangerous.")

  val tt = new HashSetPlusPlusBenchmark

  for (sharing <- List(0,20,40,60,80,90,100);
       size <- List(
         //10,100,1000,
         10000)) {
    println(s"size $size, sharing $sharing")
    tt.size = size
    tt.sharing = sharing
    tt.initKeys()
    while (true) {
      var j = 0
      val start = System.nanoTime()
      while (j < 100) {
        //      tt.opWithOverlap(bh)
        //      tt.opWithDistinct(bh)
        //      tt.opContainedWithLarge(bh)
        //      tt.opEmptyWithContained(bh)
        tt.opLargeWithContained(bh)
        //      tt.opLargeWithEmpty(bh)
        //      tt.opWithShared(bh)
        j += 1
      }
      val end = System.nanoTime()
      println((end - start)/1000000)
    }
  }
}
