package scala.reflect.internal.util

object Options {

  @inline def devTime(fn: => Unit) {
    if (ProcessSettings.developmentTime) fn
  }

}
