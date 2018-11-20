lazy val commonSettings = Seq(
    version := "0.1-SNAPSHOT"
  , scalaVersion := "2.12.7"
  , exportJars := false
  , scalacOptions += "-deprecation"
  , scalacOptions += "-feature")

lazy val root = (project in file("."))
  .aggregate(
       codeblocks
     , fib
     , methodAsFunctions
     , multiPackage
     , oop
     , parallelism
     , sort
     , splat
  )
  .settings(commonSettings: _*)
  .settings(name := "learnScala")

lazy val codeblocks = (project in file("codeblocks"))
  .settings(commonSettings: _*)
  .settings(name := "codeblocks")

lazy val fib = (project in file("fib"))
  .settings(commonSettings: _*)
  .settings(name := "fib")

lazy val methodAsFunctions = (project in file("methodAsFunctions"))
  .settings(commonSettings: _*)
  .settings(name := "methodAsFunctions")

lazy val multiPackage = (project in file("multiPackage"))
  .settings(commonSettings: _*)
  .settings(name := "multiPackage")

lazy val oop = (project in file("oop"))
  .settings(commonSettings: _*)
  .settings(name := "oop")

lazy val parallelism = (project in file("parallelism"))
  .settings(commonSettings: _*)
  .settings(name := "parallelism")

lazy val sort = (project in file("sort"))
  .settings(commonSettings: _*)
  .settings(name := "sort")

lazy val splat = (project in file("splat"))
  .settings(commonSettings: _*)
  .settings(name := "splat")

