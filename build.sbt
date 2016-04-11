lazy val commonSettings = Seq(
  organization := "mil.afrl.dtic",
  version := "1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  aggregate(firstSteps, datastructures)

lazy val firstSteps = (project in file("chap02")).
  settings(commonSettings: _*).
  settings(
    name := "firstSteps"
  )

lazy val datastructures = (project in file("chap03")).
  settings(commonSettings: _*).
  settings(
    name := "datastructures"
  )
