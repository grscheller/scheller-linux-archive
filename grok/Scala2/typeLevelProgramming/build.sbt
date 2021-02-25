lazy val root = (project in file("."))
  .settings(scalaVersion := "2.13.3")
  .settings(name := "tlp")

scalacOptions += "-deprecation"
scalacOptions += "-feature"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.3"
