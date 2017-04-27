lazy val commonSettings = Seq(
  scalaVersion := "2.12.2"
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "onejar"
  )

// Put build in Jar files if true.
exportJars := true

// Run scalac with deprecation flag
scalacOptions += "-deprecation"

