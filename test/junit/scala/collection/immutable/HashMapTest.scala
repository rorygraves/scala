package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class HashMapTest {

  private val computeHashF = {
    HashMap.empty.computeHash _
  }

  @Test
  def canMergeIdenticalHashMap1sWithNullKvs() {
    def m = new HashMap.HashMap1(1, computeHashF(1), 1, null)
    val merged = m.merged(m)(null)
    assertEquals(m, merged)
  }

  @Test
  def canMergeIdenticalHashMap1sWithNullKvsCustomMerge() {
    def m = new HashMap.HashMap1(1, computeHashF(1), 1, null)
    val merged = m.merged(m) {
      case ((k1, v1), (k2, v2)) =>
        (k1, v1 + v2)
    }
    assertEquals(new HashMap.HashMap1(1, computeHashF(1), 2, null), merged)
  }

  @Test
  def canMergeHashMap1sWithNullKvsHashCollision() {
    val key1 = 1000L * 1000 * 1000 * 10
    val key2 = key1.##.toLong
    assert(key1.## == key2.##)

    val m1 = new HashMap.HashMap1(key1, computeHashF(key1.##), 1, null)
    val m2 = new HashMap.HashMap1(key2, computeHashF(key2.##), 1, null)
    val expected = HashMap(key1 -> 1, key2 -> 1)
    val merged = m1.merged(m2)(null)
    assertEquals(expected, merged)
    val mergedWithMergeFunction = m1.merged(m2) { (kv1, kv2) =>
      throw new RuntimeException("Should not be reached.")
    }
    assertEquals(expected, mergedWithMergeFunction)
  }

  def checkBuild[K,V](distinct:Boolean, values:(K,V) * ) :Unit = {
    var map = new HashMap[K,V]
    values foreach {kv => map = map + kv}
    if (distinct) assertEquals("Test setup failure", values.size, map.size)

    val builder = HashMap.newBuilder[K,V]
    values foreach {builder += _}
    val m2 = builder.result()
    assertEquals(map.size,m2.size)

    values foreach {
      case (k, v) =>
        assertTrue(s"failed for $k $v", m2.contains(k))
        assertEquals(s"failed for $k $v", map(k), m2(k))
        assertEquals(s"failed for $k $v", map.get(k), m2.get(k))
    }
    assertEquals(map.toString,m2.toString)
    assertEquals(map,m2)

  }
  @Test
  def checkAdd(): Unit = {
    val values = (1 to 1000) map (v => (v -> s"xx $v"))
    checkBuild(true, values : _*)
  }
  @Test
  def checkSimpleAdd(): Unit = {
    checkBuild(true, (25,1), (42,"42"))
  }
}