lazy val commonSettings = Seq(
    version := "0.1-SNAPSHOT"
  , scalaVersion := "2.12.2"
  , exportJars := false
  , scalacOptions += "-deprecation")

lazy val root = (project in file("."))
  .aggregate(
       codeblocks
     , onejar
     , oop
     , parallelism
     , splat)

lazy val codeblocks = (project in file("codeblocks"))
  .settings(commonSettings)

lazy val onejar = (project in file("onejar"))
  .settings(
      commonSettings
    , test in assembly := {}
    , mainClass in assembly := Some("grockScala.HelloWorld.Main"))

lazy val oop = (project in file("oop"))
  .settings(commonSettings)

lazy val parallelism = (project in file("parallelism"))
  .settings(commonSettings)

lazy val splat = (project in file("splat"))
  .settings(commonSettings)

