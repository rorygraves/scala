#!/bin/bash
# Run this within the performance directory - compiles baseline cp and opt cp and compares the results
# you need to run genClasspathecho Compiling compiler
echo "*************************************************************"
echo "Compiling compiler:
cd .. ; sbt dist/mkPack

echo "*************************************************************"
echo "Regenerationg optimised classpath"
cd /workspace/scala/performance/ ; ./genClasspath.sh

echo "*************************************************************"
echo "Compiling base"
time /workspace/scala/build/pack/bin/scalac -cp `cat opt.classpath` -d opt-op `find src/main/scala -name "*.scala"`

echo "*************************************************************"
echo "Compiling opt"
time /workspace/scala/build/pack/bin/scalac -cp `cat opt.classpath` -d opt-op `find src/main/scala -name "*.scala"`

echo "Diffing"
cd /workspace/scala/performance/ ; diff -r opt-op base-op


#time /workspace/scala/build/pack/bin/scalac -cp /Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.1.jar:/Users/rorygraves/.ivy2/cache/com.typesafe.akka/akka-actor_2.12/jars/akka-actor_2.12-2.4.12.jar:/Users/rorygraves/.ivy2/cache/com.typesafe/config/bundles/config-1.3.0.jar:/Users/rorygraves/.ivy2/cache/org.scala-lang.modules/scala-java8-compat_2.12/bundles/scala-java8-compat_2.12-0.8.0.jar -verbose `find . -name "*.scala"` 2>&1
