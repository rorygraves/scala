package scala.tools.nsc.settings;

import scala.reflect.internal.util.AlmostFinalValue;
import scala.reflect.internal.util.BooleanContainer;

import java.lang.invoke.MethodHandle;

public class StaticSettings{

    private StaticSettings() {}

    private static final AlmostFinalValue<BooleanContainer> MACRO_DEBUG_LITE = new AlmostFinalBooleanValue();

    private static final MethodHandle MACRO_DEBUG_LITE_GETTER = MACRO_DEBUG_LITE.createGetter();

    public static boolean macroDebugLiteEnabled() {
        try {
            return ((BooleanContainer) (Object) MACRO_DEBUG_LITE_GETTER.invokeExact()).isEnabledNow();
        } catch (Throwable e) {
            throw new AssertionError(e.getMessage(), e);
        }
    }

    private static final AlmostFinalValue<BooleanContainer> MACRO_DEBUG_VERBOSE = new AlmostFinalBooleanValue();

    private static final MethodHandle MACRO_DEBUG_VERBOSE_GETTER = MACRO_DEBUG_VERBOSE.createGetter();

    public static boolean macroDebugVerboseEnabled() {
        try {
            return ((BooleanContainer) (Object) MACRO_DEBUG_VERBOSE_GETTER.invokeExact()).isEnabledNow();
        } catch (Throwable e) {
            throw new AssertionError(e.getMessage(), e);
        }
    }
    private static final AlmostFinalValue<BooleanContainer> DEBUG = new AlmostFinalBooleanValue();

    private static final MethodHandle DEBUG_GETTER = DEBUG.createGetter();

    public static boolean debugEnabled() {
        try {
            return ((BooleanContainer) (Object) DEBUG_GETTER.invokeExact()).isEnabledNow();
        } catch (Throwable e) {
            throw new AssertionError(e.getMessage(), e);
        }
    }
    private static final AlmostFinalValue<BooleanContainer> DEVELOPER = new AlmostFinalBooleanValue();

    private static final MethodHandle DEVELOPER_GETTER = DEVELOPER.createGetter();

    public static boolean developerEnabled() {
        try {
            return ((BooleanContainer) (Object) DEVELOPER_GETTER.invokeExact()).isEnabledNow();
        } catch (Throwable e) {
            throw new AssertionError(e.getMessage(), e);
        }
    }
    private static final AlmostFinalValue<BooleanContainer> PHASE_LOG = new AlmostFinalBooleanValue();

    private static final MethodHandle PHASE_LOG_GETTER = PHASE_LOG.createGetter();

    public static boolean phaseLogEnabled() {
        try {
            return ((BooleanContainer) (Object) PHASE_LOG_GETTER.invokeExact()).isEnabledNow();
        } catch (Throwable e) {
            throw new AssertionError(e.getMessage(), e);
        }
    }
    private static final AlmostFinalValue<BooleanContainer> VERBOSE = new AlmostFinalBooleanValue();

    private static final MethodHandle VERBOSE_GETTER = VERBOSE.createGetter();

    public static boolean verboseEnabled() {
        try {
            return ((BooleanContainer) (Object) VERBOSE_GETTER.invokeExact()).isEnabledNow();
        } catch (Throwable e) {
            throw new AssertionError(e.getMessage(), e);
        }
    }
    private static final AlmostFinalValue<BooleanContainer> XX = new AlmostFinalBooleanValue();

    private static final MethodHandle XX_GETTER = XX.createGetter();

    public static boolean yyEnabled() {
        try {
            return ((BooleanContainer) (Object) XX_GETTER.invokeExact()).isEnabledNow();
        } catch (Throwable e) {
            throw new AssertionError(e.getMessage(), e);
        }
    }

}

class AlmostFinalBooleanValue extends AlmostFinalValue<BooleanContainer> {
    @Override
    protected BooleanContainer initialValue() {
        return new BooleanContainer.FalseContainer();
    }
}
