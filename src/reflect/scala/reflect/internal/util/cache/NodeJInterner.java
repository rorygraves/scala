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

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicReferenceArray;

public abstract class NodeJInterner<T extends NodeJ> {

    public final int size() {
        return approxSize.get();
    }

    protected abstract T createNodeJ(String key, NodeJ node);

    public final T insertOrFind(String key) {
        int hash = key.hashCode();
        NodeJ oldTail = null;

        NodeJ node;
        do {
            //deliberately hiding this.data
            AtomicReferenceArray<NodeJ> data = initial();
            int bucket = hash & (data.length() - 1);
            NodeJ head = data.get(bucket);
            node = head;
            while ((node != null) &&
                    // we have already checked the tail
                    (node != oldTail) &&
                    // its not equal. HashCode is cheap for strings and a good discriminant
                    (node.key.hashCode() != hash || !node.key.equals(key)))
                node = node.next;
            if (node == oldTail) node = null;
            if (node == null) {
                // minor optimisation - we can skip this tail if we have to retry
                // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
                oldTail = head;
                NodeJ newNodeJ = (T) createNodeJ(key, head);
                if (data.compareAndSet(bucket, head, newNodeJ) &&
                        // volatile read to ensure that we have not grown in another thread
                        (data == this.data.get())) {
                    afterInsert(data);
                    node = newNodeJ;
                }
            } else if (
                // volatile read to ensure that we have not grown in another thread
                    data != this.data.get()) {
                node = null;
            }
        } while (node == null);
        return (T) node;
    }
//final def contains(key: String): Boolean = {
//        getExistingImpl(key) != null
//        }
//final def get(key: String): Option[T] = {
//        Option(getExistingImpl(key).asInstanceOf[T])
//        }
//final def getExistingImpl(key: String): NodeJ = {
//        val data = initial
//        val list = data.get(key.hashCode & (data.length() -1))
//        getExistingImpl(list,key)
//        }
//@tailrec private def getExistingImpl(list: NodeJ, key: String): NodeJ = {
//        if (list == null) null
//        else if (list.key == key) list
//        else getExistingImpl(list.next, key)
//        }
//

    /**
     * get the root of data
     *
     * @return
     */
    private AtomicReferenceArray<NodeJ> initial() {
        //volatile read
        AtomicReferenceArray<NodeJ> result = data.get();
        //null indicates it is in the process of being rehashed
        //updates are applied with synchronisation lock on data
        if (result == null) synchronized (data) {
            //when we have the lock we can guarantee that the other threads rehash is complete
            result = data.get();
            assert result != null;
        }
        return result;
    }

    /**
     * rehash and grow
     */
    private void afterInsert(AtomicReferenceArray<NodeJ> data) {
        int newSize = approxSize.incrementAndGet();
        int origLength = data.length();
        if (origLength < (newSize * 2)) {
            synchronized (this.data) {
                // if the value has changed already then its not our problem
                if (this.data.compareAndSet(data, null)) {
                    int size = origLength * 2;
                    int mask = origLength;
                    AtomicReferenceArray<NodeJ> newData = new AtomicReferenceArray<NodeJ>(size);

                    NodeJ head0, head1;
                    int sourceIdx = 0;
                    while (sourceIdx < origLength) {
                        head0 = head1 = null;
                        NodeJ tail0 = null, tail1 = null;
                        NodeJ sourceNode = data.get(sourceIdx);
                        while (sourceNode != null) {
                            if ((sourceNode.key.hashCode() & mask) == 0) {
                                if (head0 == null) head0 = sourceNode;
                                else tail0.next = sourceNode;
                                tail0 = sourceNode;
                            } else {
                                if (head1 == null) head1 = sourceNode;
                                else tail1.next = sourceNode;
                                tail1 = sourceNode;
                            }
                            sourceNode = sourceNode.next;
                        }
                        if (tail0 != null) tail0.next = null;
                        if (tail1 != null) tail1.next = null;
                        newData.set(sourceIdx, head0);
                        newData.set(sourceIdx + mask, head1);
                        sourceIdx += 1;
                    }
                    this.data.set(newData);
                }
            }
        }
    }


    private final AtomicInteger approxSize = new AtomicInteger();
    private final AtomicReference<AtomicReferenceArray<NodeJ>> data = new AtomicReference<>(new AtomicReferenceArray<NodeJ>(1024));

}
