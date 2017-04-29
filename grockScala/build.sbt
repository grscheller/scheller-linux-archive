lazy val commonSettings = Seq(
    version := "0.1-SNAPSHOT"
  , scalaVersion := "2.12.2"
  , exportJars := false
  , scalacOptions += "-deprecation"
  , test in assembly := {})

lazy val root = (project in file("."))
  .aggregate(
       splat
     , onejar)

lazy val splat = (project in file("splat"))
  .settings(
      commonSettings)

lazy val onejar = (project in file("onejar"))
  .settings(
      commonSettings
    , mainClass in assembly := Some("grockScala.HelloWorld.Main"))

