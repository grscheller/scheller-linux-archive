lazy val commonSettings = Seq(
  version := "0.1",
  organization := "com.scheller.geoffrey",
  scalaVersion := "2.13.3",
  exportJars := false,
  scalacOptions += "-deprecation",
  scalacOptions += "-feature",
  libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.1")

lazy val root = (project in file("."))
  .aggregate(osLibTest)
  .settings(commonSettings: _*)
  .settings(name := "osLib")

lazy val osLibTest = (project in file("osLibTest"))
  .settings(commonSettings: _*)
  .settings(name := "osLibTest")

// Based on learnScala/build.sbt
//
// To learn more about multi-project builds,
// head over to the official sbt documentation at
// http://www.scala-sbt.org/documentation.html
