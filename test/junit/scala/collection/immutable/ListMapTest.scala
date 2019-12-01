package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class ListMapTest extends AllocationTest{

  @Test
  def t7445(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5)
    assertEquals(ListMap(2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5), m.tail)
  }

  @Test
  def hasCorrectBuilder(): Unit = {
    val m = ListMap("a" -> "1", "b" -> "2", "c" -> "3", "b" -> "2.2", "d" -> "4")
    assertEquals(List("a" -> "1", "c" -> "3", "b" -> "2.2", "d" -> "4"), m.toList)
  }

  @Test
  def hasCorrectHeadTailLastInit(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3)
    assertEquals(1 -> 1, m.head)
    assertEquals(ListMap(2 -> 2, 3 -> 3), m.tail)
    assertEquals(3 -> 3, m.last)
    assertEquals(ListMap(1 -> 1, 2 -> 2), m.init)
  }

  @Test
  def hasCorrectAddRemove(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3)
    assertEquals(ListMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4), m + (4 -> 4))
    assertEquals(ListMap(1 -> 1, 3 -> 3, 2 -> 4), m + (2 -> 4))
    assertEquals(ListMap(1 -> 1, 2 -> 2, 3 -> 3), m + (2 -> 2))
    assertEquals(ListMap(2 -> 2, 3 -> 3), m - 1)
    assertEquals(ListMap(1 -> 1, 3 -> 3), m - 2)
    assertEquals(ListMap(1 -> 1, 2 -> 2, 3 -> 3), m - 4)
  }

  @Test
  def hasCorrectIterator(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4)
    assertEquals(List(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 5, 4 -> 4), m.iterator.toList)
  }

  @Test
  def nonAllocatingEmpty(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3)

    nonAllocating(m ++ ListMap.empty)
    nonAllocating(m ++ Map.empty)
    nonAllocating(ListMap.empty ++ m)
    nonAllocating(m ++ m)
  }

  @Test
  def nonAllocatingAdd(): Unit = {
    val m = ListMap(1 -> 1, 2 -> 2, 3 -> 3)
    val add = (3 -> 3)
    nonAllocating(m + add)
  }

}
