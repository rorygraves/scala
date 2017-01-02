#!/bin/bash
# From 'sbt saveClasspath' - converted 
#/Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.8.jar:/Users/rorygraves/.ivy2/cache/com.typesafe.akka/akka-actor_2.11/jars/akka-actor_2.11-2.4.12.jar:/Users/rorygraves/.ivy2/cache/com.typesafe/config/bundles/config-1.3.0.jar:/Users/rorygraves/.ivy2/cache/org.scala-lang.modules/scala-java8-compat_2.11/bundles/scala-java8-compat_2.11-0.7.0.jar

#/workspace/scala/performance/base-classpath/scala-library-2.11.8.jar:/Users/rorygraves/.ivy2/cache/com.typesafe.akka/akka-actor_2.11/jars/akka-actor_2.11-2.4.12.jar:/Users/rorygraves/.ivy2/cache/com.typesafe/config/bundles/config-1.3.0.jar:/Users/rorygraves/.ivy2/cache/org.scala-lang.modules/scala-java8-compat_2.11/bundles/scala-java8-compat_2.11-0.7.0.jar

#mkdir -p base-classpath/

#cp /Users/rorygraves/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.8.jar base-classpath/
#cp /Users/rorygraves/.ivy2/cache/com.typesafe.akka/akka-actor_2.11/jars/akka-actor_2.11-2.4.12.jar base-classpath/
#cp /Users/rorygraves/.ivy2/cache/com.typesafe/config/bundles/config-1.3.0.jar base-classpath/
#cp /Users/rorygraves/.ivy2/cache/org.scala-lang.modules/scala-java8-compat_2.11/bundles/scala-java8-compat_2.11-0.7.0.jar base-classpath/

for f in base-classpath/*.jar; do 
  fname=`basename $f`
  echo "Processing $fname file..";
  ../build/pack/bin/linkertool -in base-classpath/$fname -over -ss -out opt-classpath/$fname
 done
