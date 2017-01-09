#!/bin/bash
# Run this within the performance directory - compiles baseline cp and opt cp and compares the results
# you need to run genClasspathecho Compiling compiler
export PATH=$JAVA_HOME/bin:$PATH
wd=`pwd`
echo "*************************************************************"
echo "Compiling compiler"
cd .. ; sbt dist/mkPack

cd $wd
sbt ++2.11.8=../build/pack  compile
