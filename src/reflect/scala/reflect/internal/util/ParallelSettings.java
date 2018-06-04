package scala.reflect.internal.util;

public class ParallelSettings {
    final static boolean areAssertionsEnabled = Boolean.valueOf(System.getProperty("parallel-assertions", "true"));
}
