
import pl.project13.scala.sbt.JmhPlugin

version := "1.0"

name := "classpath-perf"

scalaVersion in ThisBuild := "2.11.8"

val saveClasspathTask = TaskKey[Unit]("saveClasspath", "Save the classpath to a file")
import sbt._
import IO._
import java.io._
saveClasspathTask := {
 val managed = (managedClasspath in Compile).value.map(_.data.getAbsolutePath)
   val unmanaged = (unmanagedClasspath in Compile).value.map(_.data.getAbsolutePath)
   println((unmanaged ++ managed).mkString(File.pathSeparator))
}

libraryDependencies += "com.typesafe.akka"                %% "akka-actor"                % "2.4.12"

