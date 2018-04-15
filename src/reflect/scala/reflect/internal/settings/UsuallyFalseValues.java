package scala.reflect.internal.settings;

import java.lang.invoke.*;

public final class UsuallyFalseValues {

    private UsuallyFalseValues() {
    }

    private static MethodHandle _true = MethodHandles.constant(Boolean.class, Boolean.TRUE);
    private static MethodHandle _false = MethodHandles.constant(Boolean.class, Boolean.FALSE);

    public static class UsuallyFalse {
        private final SwitchPoint spt = new SwitchPoint();
        private final MethodHandle worker = spt.guardWithTest(_false, _true);

        boolean value() throws Throwable{
            return (Boolean)worker.invokeExact();
        }
        public void set() {
            SwitchPoint.invalidateAll(new SwitchPoint[] {spt} );
        }
    }

    public final static UsuallyFalse debug = new UsuallyFalse();
    public final static UsuallyFalse verbose = new UsuallyFalse();
}
