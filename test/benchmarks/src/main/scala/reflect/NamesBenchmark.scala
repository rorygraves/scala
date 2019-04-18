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

package reflect

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.reflect.internal.{Names, NamesC}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Warmup(iterations = 3)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class NamesBenchmark{

  var names = new Names {}
  var namesC = new NamesC

  var someStrings: Array[String] = _

  @Setup(Level.Trial) def init(): Unit = {
    someStrings = Array.tabulate(1000)(i => s"${System.nanoTime()} $i")
    names = new Names {}
    namesC = new NamesC {}
  }

  @Benchmark def createTerm: Int = {
    someStrings map names.newTermName
    names.nameTableSize
  }
  @Benchmark def createType: Int = {
    someStrings map names.newTypeName
    names.nameTableSize
  }
  @Benchmark def createBoth: Int = {
    someStrings map (s => names.newTypeName(s).toTermName.toTypeName.toTermName)
    names.nameTableSize
  }
  @Benchmark def createTermC: Int = {
    someStrings map namesC.newTermName
    namesC.nameTableSize
  }
  @Benchmark def createTypeC: Int = {
    someStrings map namesC.newTypeName
    namesC.nameTableSize
  }
  @Benchmark def createBothC: Int = {
    someStrings map (s => namesC.newTypeName(s).toTermName.toTypeName.toTermName)
    namesC.nameTableSize
  }
}
