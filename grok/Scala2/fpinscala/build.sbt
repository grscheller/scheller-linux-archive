lazy val commonSettings = Seq(
  scalaVersion := "2.13.3"
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "fpinscala"
  )

// Put build in Jar files if true.
exportJars := false

// Run scalac with these flags
scalacOptions += "-deprecation"
scalacOptions += "-feature"
scalacOptions += "-Xsource:3"

// Tell scaladoc to include info on implicits and
// to group class members based on their semantic relationship.
scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")

