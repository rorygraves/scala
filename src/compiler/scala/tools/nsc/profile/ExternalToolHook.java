package scala.tools.nsc.profile;

/**
 * This is an external tool hook, it allows an external tool such as YourKit or JProfiler to instrument a
 * particular phase of the compiler
 *
 * The use case is like this:
 * Some other profiling has indicated that a particular phase of the compiler requires some deep analysis via an
 * external tool
 *
 * Configure the profiling tool of your choice to start and stop profiling based on execution of these methods
 *
 * when you execute scalac - add the -Yprofile-external-tool:<phase> line switch, or the matching setting.
 * -Yprofile-external-tool triggers the execution of the appropriate methods at the start or stop of a phase.
 * The parameter specifies the actualt phase to wraps with these calls, defaulting to "typer"
 */
public class ExternalToolHook {
    private ExternalToolHook() {}
    public static void before() {}
    public static void after() {}
}
