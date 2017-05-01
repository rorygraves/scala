/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.internal
package util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.reflect.NameTransformer

class FreshNameCreator(creatorPrefix: String = "") {
  protected val counters = new ConcurrentHashMap[String, AtomicLong]()

  /**
   * Create a fresh name with the given prefix. It is guaranteed
   * that the returned name has never been returned by a previous
   * call to this function (provided the prefix does not end in a digit).
   */
  def newName(prefix: String): String = {
    val safePrefix = NameTransformer.encode(prefix)
    var sharedCounter = counters.get(safePrefix)
    if (sharedCounter eq null) {
      val newCounter = new AtomicLong(0)
      sharedCounter = counters.putIfAbsent(safePrefix, newCounter)
      if (sharedCounter eq null)
        sharedCounter = newCounter
    }
    val idx = String.valueOf(sharedCounter.incrementAndGet())

    //return effectively s"$creatorPrefix$safePrefix$idx", but less garbage

    val res = new Array[Char](creatorPrefix.length + safePrefix.length + idx.length)
    creatorPrefix.getChars(0,creatorPrefix.length, res, 0)
    prefix.getChars(0,prefix.length, res, creatorPrefix.length)
    idx.getChars(0,idx.length, res, creatorPrefix.length+creatorPrefix.length)
    new String(res)
  }
}
