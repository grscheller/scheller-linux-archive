lazy val commonSettings = Seq(
  scalaVersion := "2.12.1"
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "fpinscala"
  )

// Put build in Jar files if true.
exportJars := false

// Run scalac with deprecation flag
scalacOptions += "-deprecation"

// Tell scaladoc to process author tags
scalacOptions in (Compile,doc) += "-author"

// Tell scaladoc to include info on implicits and 
// to group class members based on their semantic relationship.
scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")

// Force sbt to use a local version of scala
//    Use case - when not available in a repo
// scalaHome := Some(file("/usr/share/scala/"))
