cd /workspace/scala ; sbt dist/mkPack
cd /workspace/scala/performance/src/main/scala/
time /workspace/scala/build/pack/bin/scalac -cp /Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.1.jar:/Users/rorygraves/.ivy2/cache/com.typesafe.akka/akka-actor_2.12/jars/akka-actor_2.12-2.4.12.jar:/Users/rorygraves/.ivy2/cache/com.typesafe/config/bundles/config-1.3.0.jar:/Users/rorygraves/.ivy2/cache/org.scala-lang.modules/scala-java8-compat_2.12/bundles/scala-java8-compat_2.12-0.8.0.jar -verbose `find . -name "*.scala"` 2>&1
