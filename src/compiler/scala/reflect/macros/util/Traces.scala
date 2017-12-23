package scala.reflect.macros
package util

import scala.tools.nsc.settings.StaticSettings

trait Traces {
  def globalSettings: scala.tools.nsc.Settings

  val macroDebugLite = StaticSettings.macroDebugLiteEnabled() && globalSettings.YmacrodebugLite.value
  val macroDebugVerbose = StaticSettings.macroDebugVerboseEnabled() && globalSettings.YmacrodebugVerbose.value
  @inline final def macroLogLite(msg: => Any) { if (
    (StaticSettings.macroDebugVerboseEnabled() || StaticSettings.macroDebugVerboseEnabled())
      && (macroDebugLite || macroDebugVerbose))
    println(msg) }
  @inline final def macroLogVerbose(msg: => Any) { if (StaticSettings.macroDebugVerboseEnabled() && macroDebugVerbose) println(msg) }
}
