lazy val commonSettings = Seq(
    version := "0.1-SNAPSHOT"
  , scalaVersion := "2.12.4"
  , exportJars := false
  , scalacOptions += "-deprecation")

lazy val root = (project in file("."))
  .aggregate(
       codeblocks
     , grok
     , oop
     , parallelism
     , sort
     , splat
  )
  .settings(commonSettings: _*)
  .settings(name := "grokScala")

lazy val codeblocks = (project in file("codeblocks"))
  .settings(commonSettings: _*)
  .settings(name := "codeblocks")

lazy val grok = (project in file("grok"))
  .settings(commonSettings: _*)
  .settings(name := "grok")

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
