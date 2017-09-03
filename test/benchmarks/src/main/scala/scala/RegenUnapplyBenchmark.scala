package scala

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.matching.Regex
import scala.collection.JavaConverters._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class RegenUnapplyBenchmark {

  /**
    * The idea is borrowed from: https://rust-leipzig.github.io/regex/2017/03/28/comparison-of-regex-engines/
    * the corpus is downloaded from: https://github.com/rust-leipzig/regex-performance/blob/master/3200.txt
    */
/*  val corpus = {
//    val url = classOf[RegenUnapplyBenchmark].getClassLoader.getResource("3200.txt")
    val path = Paths.get("./src/main/resources/3200.txt")
    Files.readAllLines(path, StandardCharsets.UTF_8).asScala.mkString(" ")
  }*/

  /**
    * pre-compile regex at class constructor to save iteration time
    */
  val t8022CharSequenceRegex = """.*: (.)$""".r
  val t8022MatchRegex = """(\d)""".r
  val t8787nullMatchRegex = """\d+""".r
  val t8787nullMatcherRegex = """(\d+):(\d+)""".r

  @Param(Array("1", "10", "100", "1000"))
  var groupCount: Int = _
  var groupCorpus: String = _
  var groupPattern: Regex = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    groupCorpus = List.tabulate(groupCount)(idx => s"$idx:$idx").mkString(" ")
    groupPattern = List.tabulate(groupCount)(_ => """(\d+:\d+)""").mkString(" ").r
  }

  @Benchmark def t8022CharSequence(bh: Blackhole): Unit = {
    val full = t8022CharSequenceRegex
    val text = "   When I use this operator: *"
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val x :: Nil = full.unapplySeq(text: Any).get
    val y :: Nil = full.unapplySeq(text: CharSequence).get
    require(x == "*")
    require(y == "*")
    bh.consume(x)
    bh.consume(y)
  }

  @Benchmark def t8022Match(bh: Blackhole): Unit = {
    val R = t8022MatchRegex
    val matchh = R.findFirstMatchIn("a1").get
    // Testing 2.10.x compatibility of the return types of unapplySeq
    val x :: Nil = R.unapplySeq(matchh: Any).get
    val y :: Nil = R.unapplySeq(matchh).get
    require(x == "1")
    require(y == "1")
    bh.consume(x)
    bh.consume(y)
  }

  @Benchmark def t8787nullMatch(bh: Blackhole) = {
    val r = t8022MatchRegex
    val s: String = null
    val x = s match { case r() => 1 ; case _ => 2 }
    require(x == 2)
    bh.consume(x)
  }

  @Benchmark def t8787nullMatcher(bh: Blackhole) = {
    val r = t8787nullMatcherRegex
    val s = "1:2 3:4 5:6"
    val z = ((r findAllMatchIn s).toList :+ null) flatMap {
      case r(x, y) => Some((x.toInt, y.toInt))
      case _       => None
    }

    require(List((1,2),(3,4),(5,6)) == z)
    bh.consume(z)
  }

  @Benchmark def groupingBenchmark(bh: Blackhole) = {
    val r = groupPattern

    val res = groupCorpus match {
      case r(all @ _*) => all
      case _ => null
    }

    require(null != res)
    bh.consume(res)
  }

  /*@Benchmark def heavyMatching(bh: Blackhole) = {
    val regexs = List(
      "(Twain)",
      "((?i)Twain)",
      "([a-z]shing)",
      "(Huck[a-zA-Z]+|Saw[a-zA-Z]+)",
      "(\\b\\w+nn\\b)",
      "([a-q][^u-z]{13}x)",
      "(Tom|Sawyer|Huckleberry|Finn)",
      "((?i)Tom|Sawyer|Huckleberry|Finn)",
      "(.{0,2}(Tom|Sawyer|Huckleberry|Finn))",
      "(.{2,4}(Tom|Sawyer|Huckleberry|Finn))",
      "(Tom.{10,25}river|river.{10,25}Tom)",
      "([a-zA-Z]+ing)",
      "(\\s[a-zA-Z]{0,12}ing\\s)",
      "([A-Za-z]awyer|[A-Za-z]inn)\\s",
      "([\"'][^\"']{0,30}[?!\\.][\"'])",
      "(\u221E|\u2713)",
      "(\\p{Sm})"
    ).map(_.r.unanchored)

    val res = regexs.map (r => r.findAllMatchIn(corpus).toList flatMap {
      case r(all @ _*) => all
    })

    bh.consume(res)
  }*/
}