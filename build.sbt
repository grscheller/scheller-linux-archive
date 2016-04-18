lazy val commonSettings = Seq(
  organization := "mil.afrl.dtic",
  version := "1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*)
