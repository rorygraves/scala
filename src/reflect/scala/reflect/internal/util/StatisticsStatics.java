package scala.reflect.internal.util;

import scala.reflect.internal.util.AlmostFinalValue;
import java.lang.invoke.MethodHandle;

/**
 * Represents all the simulated statics for Statistics.
 * 
 * Its implementation delegates to {@link scala.reflect.internal.util.AlmostFinalValue},
 * which helps performance (see docs to find out why).
 */
public final class StatisticsStatics {

  public final static boolean areSomeColdStatsEnabled = ProcessSettings.coldStatsEnabled;
  public final static boolean areSomeHotStatsEnabled = ProcessSettings.hotStatsEnabled;
}