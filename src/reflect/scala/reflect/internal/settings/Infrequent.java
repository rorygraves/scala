package scala.reflect.internal.settings;

import scala.reflect.internal.util.AlmostFinalValue;
import scala.reflect.runtime.Settings;

import java.lang.invoke.MethodHandle;

public class Infrequent {
    private final static class AlmostFinalBoolean extends AlmostFinalValue<Boolean> {
        static Object[] NO_ARGS = new Object[0];
        @Override
        protected Boolean initialValue() {
            return false;
        }

        public MethodHandle getter = createGetter();

        final boolean get() {
            getter.invokeWithArguments(NO_ARGS);
        }
    }

    private Infrequent () {}

    private static AlmostFinalBoolean debugValue = new AlmostFinalBoolean();

    static boolean hasAnyDebug() {
        return debugValue.;
    }
}
