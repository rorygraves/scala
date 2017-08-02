package scala.collection

import org.junit._
import org.junit.Assert._

class MapIdenticalTest {

  import AbstractMap.identical

  @Test
  def testBooleans(): Unit = {
    assertTrue(identical(true, true))
    assertTrue(identical(false, false))
    assertFalse(identical(false, true))
    assertFalse(identical(true, false))

    assertTrue(identical(true.booleanValue(), true))
    assertTrue(identical(Boolean.box(true), Boolean.box(true)))
    assertTrue(identical(Boolean.box(false), Boolean.box(false)))

    assertFalse(identical(null, true))
    assertFalse(identical(null, false))
    assertFalse(identical(true, null))
    assertFalse(identical(false, null))
  }

  @Test
  def testNull(): Unit = {
    assertTrue(identical(null, null))

    assertFalse(identical(false, null))
    assertFalse(identical(true, null))
    assertFalse(identical(null, false))
    assertFalse(identical(null, true))

    assertFalse(identical("", null))
    assertFalse(identical(None, null))
    assertFalse(identical(Nil, null))
    assertFalse(identical(1, null))
    assertFalse(identical(Double.NaN, null))
    assertFalse(identical(1.0, null))
    assertFalse(identical(true, null))

    assertFalse(identical(null, ""))
    assertFalse(identical(null, None))
    assertFalse(identical(null, Nil))
    assertFalse(identical(null, 1))
    assertFalse(identical(null, Double.NaN))
    assertFalse(identical(null, 1.0))
    assertFalse(identical(null, true))
  }

  @Test
  def testByte(): Unit = {
    for (i <- List(-1, 0, 1, 2, 4, 10)) {
      assertTrue(s"$i", identical(i.toByte, i.toByte))
      assertFalse(s"$i", identical(i.toByte, (i + 1).toByte))
      assertFalse(s"$i", identical(i.toByte, (i - 1).toByte))
    }
  }

  @Test
  def testShort(): Unit = {
    for (i <- List(-1, 0, 1, 2, 4, 10)) {
      assertTrue(s"$i", identical(i.toShort, i.toShort))
      assertFalse(s"$i", identical(i.toShort, (i + 1).toShort))
      assertFalse(s"$i", identical(i.toShort, (i - 1).toShort))
    }
  }

  @Test
  def testInt(): Unit = {
    for (i <- List(-1, 0, 1, 2, 4, 10)) {
      assertTrue(s"$i", identical(i.toInt, i.toInt))
      assertFalse(s"$i", identical(i.toInt, (i + 1).toInt))
      assertFalse(s"$i", identical(i.toInt, (i - 1).toInt))
    }
  }

  @Test
  def testLong(): Unit = {
    for (i <- List(-1, 0, 1, 2, 4, 10)) {
      assertTrue(s"$i", identical(i.toLong, i.toLong))
      assertFalse(s"$i", identical(i.toLong, (i + 1).toLong))
      assertFalse(s"$i", identical(i.toLong, (i - 1).toLong))
    }
  }

  @Test
  def testFloat(): Unit = {
    for (i <- List(-1, 0, 1, 2, 4, 10)) {
      assertTrue(s"$i", identical(i.toFloat, i.toFloat))
      assertFalse(s"$i", identical(i.toFloat, (i + 1).toFloat))
      assertFalse(s"$i", identical(i.toFloat, (i - 1).toFloat))
    }
  }

  @Test
  def testDouble(): Unit = {
    for (i <- List(-1, 0, 1, 2, 4, 10)) {
      assertTrue(s"$i", identical(i.toDouble, i.toDouble))
      assertFalse(s"$i", identical(i.toDouble, (i + 1).toDouble))
      assertFalse(s"$i", identical(i.toDouble, (i - 1).toDouble))
    }
  }

  @Test
  def testNaN(): Unit = {
    assertTrue(identical(Double.NaN, Double.NaN))
    assertTrue(identical(Float.NaN, Float.NaN))

    assertFalse(identical(Float.NaN, Double.NaN))
    assertFalse(identical(Double.NaN, Float.NaN))
  }
}
