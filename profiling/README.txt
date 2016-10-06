To profile a given compilation module

1) Download https://github.com/retronym/scala-jmh-suite
2) copy SbtArgsFilePlugin.scala ~/.sbt/0.13/plugins/SbtArgsFilePlugin.scala 
3) In the target compilation unit run compile:argsFilePrint
   This outputs the scalac commandline file needed for jhm
e.g.
-classpath /Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.8.jar:/Users/rorygraves/.ivy2/cache/args4j/args4j/bundles/args4j-2.33.jar:/Users/rorygraves/.ivy2/cache/org.ow2.asm/asm/jars/asm-5.1.jar /workspace/scala/jarshrink/src/main/scala/scala.tools.jarshrink/JarShrink.scala

In akka project
    sbt akka-actor/argsFilePrint > /tmp/akka.config

(in scala-jmh-suite project' 

sbt 'compilation/jmh:run ColdScalacBenchmark -p source=@/workspace/scala/profiling/akka-core.config -p _scalaVersion="2.11.8"' 
vs
sbt ++2.11.8=/workspace/scala/build/pack 'compilation/jmh:run ColdScalacBenchmark -p source=@/workspace/scala/profiling/akka-core.config -p _scalaVersion="2.11.8"'

