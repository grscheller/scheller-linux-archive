// Using SBT version 0.13 as the build tool.
// 

lazy val commonSettings = Seq(
  organization := "geoffrey.scheller.com",
  version := "1.1",
  scalaVersion := "2.12.1"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*)

// Put build in Jar files if true.
exportJars := false

// Run scalac with deprecation flag
scalacOptions += "-deprecation"

// Tell scaladoc to process author tags
scalacOptions in (Compile,doc) += "-author"

// Tell scaladoc to include info on implicits and 
// to group class members based on their semantic relationship.
scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits")

// Build to require/target java8
javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")
scalacOptions += "-target:jvm-1.8"

// Force the use of java8 libraries only
initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8")
    sys.error("Java 8 is required for this project.")
}

// Force sbt to use a local version of scala
//    Use case - when not available in a repo
// scalaHome := Some(file("/home/schelleg/opt/scala-2.12.0-M4/"))
