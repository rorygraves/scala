package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.{Parameter, Parameters}

object HashMapCombinationTest {

  case class RunInfo(steps: Seq[(Step, Boolean)]) {
    def desc = {
      val names = steps map {
        case (step, verify) => s"${step.name}:V=${if (verify) "Y" else "N"}"
      }
      names.mkString(";")
    }
  }

  case class Step(name: String, fn: (Map[Any, Any], HashMap.HashMapBuilder[Any, Any]) => (Map[Any, Any], Seq[Seq[Any]]))

  val initialEmpty = Step("Initially empty", noop _)
  val initialPopulatedString = Step("Initially string values", populateString _)
  val initialPopulatedId = Step("Initially id values", populateId _)
  val initialSteps = List(initialEmpty, initialPopulatedString, initialPopulatedId)

  @Parameters(name = "{index} : {1}")
  def buildRuns(): java.util.List[Array[AnyRef]] = {
    import collection.JavaConverters._
    val allRuns = for (initial <- initialSteps; initialVerify <- List(true, false))
      yield RunInfo(List((initial, initialVerify)))

    (allRuns map { info => Array[AnyRef](info, info.desc) }).asJava
  }


  class CollidingHashes(val hash: Int, val value: Any) {
    override def hashCode(): Int = hash

    override def equals(obj: scala.Any): Boolean = obj match {
      case c: CollidingHashes => c.hash == hash && c.value == value
      case _ => false
    }
  }

  val someCollisions1 = for (i <- 1 to 100) yield {
    new CollidingHashes(1, s"Colliding(1, ${i})")
  }
  val someCollisions2 = for (i <- 1 to 100) yield {
    new CollidingHashes(1, s"Colliding(1, ${i})")
  }

  def noop(expected: Map[Any, Any], underTest: HashMap.HashMapBuilder[Any, Any]) = (expected, Nil)

  def populateString(expected: Map[Any, Any], underTest: HashMap.HashMapBuilder[Any, Any]) = {
    val range = List(-10000 to 10000, List(true, false, 0.0D, 0L, 0F, "Foo", "Bar"), someCollisions1, someCollisions2)
    var exp = expected
    for (seq <- range; key <- seq) {
      exp = `both+=`(exp, underTest, key, s"populate - $key")
    }
    (exp, range)
  }

  def populateId(expected: Map[Any, Any], underTest: HashMap.HashMapBuilder[Any, Any]) = {
    val range = List(-10000 to 10000, List(true, false, 0.0D, 0L, 0F, "Foo", "Bar"), someCollisions1, someCollisions2)
    var exp = expected
    for (seq <- range; key <- seq) {
      exp = `both+=`(exp, underTest, key, key)
    }
    (exp, range)
  }

  def `both+=`(expected: Map[Any, Any], underTest: HashMap.HashMapBuilder[Any, Any], key: Any, value: Any): Map[Any, Any] = {
    var exp = expected
    exp = exp -key + (key -> value)
    underTest += (key -> value)
    exp
  }

  def `both++=`(expected: Map[Any, Any], underTest: HashMap.HashMapBuilder[Any, Any], values: Traversable[(Any, Any)]): Map[Any, Any] = {
    var exp = expected
    //we dont use ++ on expected as that uses a builder internally
    values foreach { kv => exp = exp - kv._1 + kv }
    underTest ++= values
    exp
  }

  def `both-=`[K, V](expected: Map[Any, Any], underTest: HashMap.HashMapBuilder[K, V], key: K): Map[Any, Any] = {
    var exp = expected
    exp = exp - key
    underTest -= key
    exp
  }

  //  def `both--=` [K,V](expected:Map[K,V], underTest:HashMap.HashMapBuilder[K,V], values: Traversable[K]) : Map[K,V] = {
  //    var exp = expected
  //    //we dont use ++ on expected as that uses a builder internally
  //    values foreach{ k => exp = exp - k}
  //    underTest --= values
  //    exp
  //  }
}

@RunWith(classOf[Parameterized])
class HashMapCombinationTest(
                              val runInfo: HashMapCombinationTest.RunInfo,

                              /**
                                * This seems unused - but is used to name the test - see above
                                * {{{  @Parameters(name="{index} : {1}")
                                * def buildRuns(): java.util.List[Array[AnyRef]] = ... }}}
                                */
                              val desc: String) {

  import HashMapCombinationTest._
  def checkBuild(range: scala.Iterable[Any], expected: Map[Any, Any], underTest: HashMap.HashMapBuilder[Any, Any]): Unit = {

    val m2 = underTest.result()
    assertEquals(expected.size, m2.size)

    expected foreach {
      case (k, v) =>
        assertTrue(s"failed for $k -> $v", m2.contains(k))
        assertEquals(s"failed for $k -> $v", expected(k), m2(k))
        assertEquals(s"failed for $k -> $v", expected.get(k), m2.get(k))
    }
    range.filterNot(expected.keySet) foreach {
      absent =>
        assertFalse(s"contains failed at (expected missed) key $absent", m2.contains(absent))
        assertEquals(s"get failed at (expected missed) key $absent", None, m2.get(absent))
    }
    assertEquals(expected.toString, m2.toString)
    assertEquals(expected, m2)


  }

  @Test def checkCombinations() = {
    var expected = Map.empty[Any, Any]
    val underTest = HashMap.newBuilder[Any, Any]

    val range = new collection.mutable.HashSet[Any]()

    for ((step, check) <- runInfo.steps) {
      val (res, ranges) = step.fn(expected, underTest)
      ranges foreach {
        range ++= _
      }
      expected = res
      if (check) checkBuild(range, expected, underTest)
    }
    //always check the end result
    checkBuild(range, expected, underTest)

  }

  //  @Test
  //  def checkBuildAddBasic(): Unit = checkBuildAdd(true)
  //
  //  @Test
  //  def checkBuildAddReuse(): Unit = checkBuildAdd(false)
  //
  //  def checkBuildAdd(intermediate: Boolean): Unit = {
  //    var expected = Map.empty[Int, String]
  //    val underTest = HashMap.newBuilder[Int, String]
  //    val range = 1 to 2000
  //
  //    (1 to 1500) foreach { value =>
  //      expected = `both+=`(expected, underTest, value, s"xx $value")
  //    }
  //    if (intermediate) checkBuild(range, expected, underTest)
  //
  //    (1000 to 2000) foreach { value =>
  //      expected = `both+=`(expected, underTest, value, s"xx $value")
  //    }
  //    checkBuild(range, expected, underTest)
  //
  //  }
  //
  //  @Test
  //  def checkBuildAddCombinations(): Unit = {
  //    for (intermediate <- List(true, false);
  //         bulk <- List(noop _, toMap _, toHashMap _)) {
  //      checkBuildAddCombinations(intermediate, bulk)
  //    }
  //  }
  //
  //  def noop(in: Seq[(Int, String)]): Traversable[(Int, String)] = in
  //
  //  // too a map without using a builder
  //  def toMap(in: Seq[(Int, String)]): Traversable[(Int, String)] = {
  //    var acc = Map.empty[Int, String]
  //    for (x <- in) acc += x
  //    acc
  //  }
  //
  //  // too a hashmap without using a builder
  //  def toHashMap(in: Seq[(Int, String)]): Traversable[(Int, String)] = {
  //    var acc = HashMap.empty[Int, String]
  //    for (x <- in) acc += x
  //    acc
  //  }
  //  def checkBuildAddCombinations(intermediate: Boolean, bulkAdd:Seq[(Int, String)] => Traversable[(Int, String)]): Unit = {
  //    var expected = Map.empty[Int,String]
  //    val underTest = HashMap.newBuilder[Int,String]
  //    val range = 1 to 2000
  //
  //    (1 to 1500) foreach{ value =>
  //      expected = `both+=`(expected, underTest, value, s"xx $value")
  //    }
  //    if (intermediate) checkBuild(range, expected,underTest)
  //
  //    val bulk = bulkAdd((1000 to 2000) map { value => (value, s"yy $value")})
  //
  //    expected = `both++=`(expected,underTest, bulk)
  //
  //    if (intermediate) checkBuild(range, expected,underTest)
  //
  //    (500 to 1500) foreach{ value =>
  //      expected = `both-=`(expected, underTest, value)
  //    }
  //    checkBuild(range, expected,underTest)
  //  }


}


