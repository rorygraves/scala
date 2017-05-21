#!/bin/bash
# Run this within the performance directory - compiles baseline cp and opt cp and compares the results
# you need to run genClasspathecho Compiling compiler
echo "*************************************************************"
echo "Compiling compiler"
cd .. ; sbt dist/mkPack

echo "*************************************************************"
echo "Regenerationg optimised classpath"
cd /workspace/scala/performance/ ; ./genClasspath.sh

#echo "*************************************************************"
#echo "Compiling base"
#mkdir -p base-op/
#time /workspace/scala/build/pack/bin/scalac -cp `cat base.classpath` -d base-op `find src/main/scala -name "*.scala"` `find src/main/scala -name "*.java"`

echo "*************************************************************"
echo "Compiling opt"
mkdir -p opt-op/

time /workspace/scala/build/pack/bin/scalac -cp `cat opt.classpath` -d opt-op `find src/main/scala -name "*.scala"`  `find src/main/scala -name "*.java"`

echo "Diffing"
cd /workspace/scala/performance/ ; diff -r opt-op base-op


