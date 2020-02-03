package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class HashMapTest extends AllocationTest {

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

  @Test
  def canMergeHashMapCollision1WithCorrectMerege() {
    case class A(k: Int) {
      override def hashCode = 0
    }
    val m1 = HashMap(A(0) -> 2, A(1) -> 2)
    val m2 = HashMap(A(0) -> 1, A(1) -> 1)
    val merged = m1.merged(m2) { case ((k, l), (_, r)) => k -> (l - r) }
    val expected = HashMap(A(0) -> 1, A(1) -> 1)
    assertEquals(merged, expected)
  }

  @Test
  def t11257(): Unit = {
    case class PoorlyHashed(i: Int) {
      override def hashCode(): Int = i match {
        case 0 | 1 => 42
        case _ => super.hashCode()
      }
    }
    val hashMapCollision = HashMap(PoorlyHashed(0) -> 0, PoorlyHashed(1) -> 1)
    val singleElementMap = hashMapCollision.split.head
    assert(singleElementMap.isInstanceOf[HashMap.HashMap1[_, _]])
    val stillSingleElement = singleElementMap.split.head
    assert(stillSingleElement.isInstanceOf[HashMap.HashMap1[_, _]])
    val twoElemTrie = stillSingleElement + (PoorlyHashed(2) -> 2)
    assert(twoElemTrie.isInstanceOf[HashMap.HashTrieMap[_, _]])
  }

  def generate(): HashMap[String, String] = {
    (1 to 1000).map { i => s"key $i" -> s"value $i" }(scala.collection.breakOut)
  }

  @Test
  def nonAllocatingIdentical(): Unit = {
    val base = generate()
    assertTrue(nonAllocating {
      base == base
    })
  }

  @Test
  def nonAllocatingNotShared(): Unit = {
    val base = generate()
    val notShared = generate()

    assertTrue(nonAllocating {
      base == notShared
    })
    assertTrue(nonAllocating {
      notShared == base
    })
  }

  @Test
  def nonAllocatingShared(): Unit = {
    val base = generate()
    val shared = (base - base.head._1) + base.head

    assertTrue(nonAllocating {
      base == shared
    })
    assertTrue(nonAllocating {
      shared == base
    })
  }
}
