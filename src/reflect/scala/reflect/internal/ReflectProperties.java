package scala.reflect.internal;

public class ReflectProperties {
    private ReflectProperties() {
    }

    public final static boolean GlbLubs_printLubs;
    public final static boolean JavaUniverse_isLogging;
    public final static boolean SymbolTable_traceSymbolActivity;
    public final static boolean Types_propagateParameterBoundsToTypeVars;
    public final static boolean Types_sharperSkolems;

    static {
        java.util.Properties props = System.getProperties();

        GlbLubs_printLubs = props.containsKey("scalac.debug.lub");
        JavaUniverse_isLogging = props.containsKey("scala.debug.reflect");
        SymbolTable_traceSymbolActivity = props.containsKey("scalac.debug.syms");
        Types_propagateParameterBoundsToTypeVars = props.containsKey("scalac.debug.prop-constraints");
        Types_sharperSkolems = props.containsKey("scalac.experimental.sharper-skolems");

    }
}
