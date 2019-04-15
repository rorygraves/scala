/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.internal.util.cache;

public abstract class NodeJ {
    final String key;
    volatile NodeJ next;

    protected NodeJ(String key, NodeJ next) {
        this.key = key;
        this.next = next;
    }
}

