To profile a given compilation module

1) Download https://github.com/retronym/scala-jmh-suite
2) copy SbtArgsFilePlugin.scala ~/.sbt/0.13/plugins/SbtArgsFilePlugin.scala 
3) In the target compilation unit run compile:argsFilePrint
   This outputs the scalac commandline file needed for jhm
e.g.
-classpath /Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.8.jar:/Users/rorygraves/.ivy2/cache/args4j/args4j/bundles/args4j-2.33.jar:/Users/rorygraves/.ivy2/cache/org.ow2.asm/asm/jars/asm-5.1.jar /workspace/scala/jarshrink/src/main/scala/scala.tools.jarshrink/JarShrink.scala

In akka project
    sbt akka-actor/argsFilePrint > /tmp/akka.config

(in scala-jmh-suite project use rory's fork until changes are merged) 

sbt 'compilation/jmh:run ColdScalacBenchmark -p source=@/workspace/scala/profiling/akka-core.config -p _scalaVersion="2.11.8"' 
vs
sbt ++2.11.8=/workspace/scala/build/pack 'compilation/jmh:run ColdScalacBenchmark -p source=@/workspace/scala/profiling/akka-core.config -p _scalaVersion="2.11.8"'

To run JMH profiling
sbt  'compilation/jmh:run ColdScalacBenchmark -prof jmh.extras.JFR -i 1 -wi 0 -p source=@/workspace/scala/profiling/akka-core-compressed.config -p _scalaVersion="2.11.8"'

N.b. at the moment the -i and -wi flags appear to be ignored meaning it runs 16 copies and generates 10 jfr files.
Open them up in jmc (java mission control) to explore.

Todos:
1) Fix the config flags for JFR profiling to make it quicker
2) Work out how to set JFR config file so we can switch on various options

