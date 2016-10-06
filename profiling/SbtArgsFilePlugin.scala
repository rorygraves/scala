package io.github.retronym

import sbt._
import Keys._

object SbtArgsFilePlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = sbt.plugins.JvmPlugin
  
  val argsFileContents = taskKey[String]("Contents file suitable for `scalac @args.txt`")
  val argsFilePrint = taskKey[Unit]("Show contents file suitable for `scalac @args.txt`")
  val cpPrint = taskKey[Unit]("Show contents file suitable for `scalac @args.txt`")
override lazy val projectSettings = List(Compile, Test).flatMap(c => inConfig(c)(Seq(
  
    argsFileContents := {
      ("-cp " + ((dependencyClasspath in Compile).value.files.mkString(":")))  + "\n" + 
      (scalacOptions.value ++ sources.value).mkString("\n")
    },
    argsFilePrint := println(argsFileContents.value)
  )))
}
