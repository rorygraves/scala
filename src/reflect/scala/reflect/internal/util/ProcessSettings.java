package scala.reflect.internal.util;

/**
 * final static static values that are used to enable th JIT to elide features
 * Usually used to disable and enable setting that are specific to development of the scala libraries, or some
 * specific run configuration.
 */
public class ProcessSettings {
    private final static String prefix = "scala.settings.";

    private final static boolean getBoolean(String name, boolean def) {
        String setting = prefix + name;
        String raw = System.getProperty(setting);
        if (raw == null) return def;
        if (raw.equals("true")) return true;
        if (raw.equals("false")) return false;
        throw new IllegalArgumentException("Cant parse " + raw + " as a bolean for " + setting);
    }

    public final static boolean hotStatsEnabled = getBoolean("hotStatsEnabled", false);
    public final static boolean coldStatsEnabled = getBoolean("coldStatsEnabled", false);

    public final static boolean developmentTime = getBoolean("developmentTime", false);
    public final static boolean debug = getBoolean("debug", false);

}
