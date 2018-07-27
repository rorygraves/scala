package scala.collection.immutable

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class VectorTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val v = Vector(0) ++ Vector(1 to 64: _*)

    assertEquals(Vector(0, 1), v take 2)
    assertEquals(Vector(63, 64), v takeRight 2)
    assertEquals(Vector(2 to 64: _*), v drop 2)
    assertEquals(Vector(0 to 62: _*), v dropRight 2)

    assertEquals(v, v take Int.MaxValue)
    assertEquals(v, v takeRight Int.MaxValue)
    assertEquals(Vector.empty[Int], v drop Int.MaxValue)
    assertEquals(Vector.empty[Int], v dropRight Int.MaxValue)

    assertEquals(Vector.empty[Int], v take Int.MinValue)
    assertEquals(Vector.empty[Int], v takeRight Int.MinValue)
    assertEquals(v, v drop Int.MinValue)
    assertEquals(v, v dropRight Int.MinValue)
  }

  @Test
  def hasCorrectPrependedAll(): Unit = {
    val els = Vector(1 to 1000: _*)

    for (i <- 0 until els.size) {
      val (prefix, suffix) = els.splitAt(i)

      assertEquals(els, prefix ++: suffix)
      assertEquals(els, prefix.toList ++: suffix)
    }
  }

  @Test def checkSearch: Unit = SeqTests.checkSearch(Vector(0 to 1000: _*), 15,  implicitly[Ordering[Int]])


  @Test
  def vectorIteratorDrop(): Unit = {
    val underlying = Vector(0 to 10010: _*)

    val totalSize = underlying.size

    for (start <- 1056 to 10000) {
      val it = underlying.iterator.drop(start)
      assertEquals(totalSize - start, it.knownSize)
      assertEquals(totalSize - start, it.size)
      assertTrue(it.hasNext)
      assertEquals(start, it.next())
    }
  }
  def intercept[T <: Throwable: Manifest](fn: => Any): T = {
    try {
      fn
      fail(s"expected a ${manifest[T].runtimeClass.getName} to be thrown")
      ???
    } catch {
      case x: T => x
    }
  }
  @Test
  def vectorIteratorDropToEnd(): Unit = {
    val underlying = Vector(0)

    for (start <- List(1,2,3,4,99)) {
      {
        val it = underlying.iterator.drop(start)
        assertFalse(it.hasNext)
        intercept[NoSuchElementException](it.next)
        it.drop(0)
        assertFalse(it.hasNext)
        it.drop(1)
        assertFalse(it.hasNext)
        it.drop(99)
        assertFalse(it.hasNext)
        intercept[NoSuchElementException](it.next)
      }

      {
        val it = underlying.iterator.drop(start)
        intercept[NoSuchElementException](it.next)
        it.drop(0)
        it.drop(1)
        it.drop(99)
        intercept[NoSuchElementException](it.next)
      }
    }
  }
  @Test
  def vectorIteratorRepeated(): Unit = {
    val underlying = Vector(1 to 10001: _*)


    for (stepSize <- List(0, 1, 2, 3, 4, 8, 10, 24, 32, 63, 64, 100)) {
      val it = underlying.iterator
      for (stepCount <- 1 to 10) {
        it.drop(stepSize)
        assertTrue(it.hasNext)
        val expected = (stepSize + 1) * stepCount
        assertEquals(expected, it.next())
      }
    }
  }
}
