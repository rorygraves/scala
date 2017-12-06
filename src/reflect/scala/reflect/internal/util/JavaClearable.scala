package scala.reflect.internal.util

import scala.collection.generic.Clearable
import java.util.{Map => JMap, Collection => JCollection}

object JavaClearable {
  def apply(data: JCollection[_]) = new JavaClearable(data)
  def apply(data: JMap[_,_]) = new JavaClearable(data.keySet())
}
class JavaClearable private (private val data: JCollection[_]) extends Clearable {
  override def clear(): Unit = data.clear

  override def hashCode() = data.hashCode()

  override def equals(obj: scala.Any) = obj match {
    case jc: JavaClearable => jc.data eq data
    case _ => false
  }
}
