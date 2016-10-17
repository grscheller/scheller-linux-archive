// Using SBT version 0.13 as the build tool.
// 
//   Using the sbteclipse-plugin 4.0.0 to generate Eclipse
//   configuation files.
//
//   Configured Eclipse IDE for Java Developers, version Mars-2,
//   release 4.5.2 with the scala-ide.org Scala Plugin, 
//   version 4.4.1-v2 for code editing.
//   
//   Vim is still my preferred editing tool!
//   

lazy val commonSettings = Seq(
  organization := "geoffrey.scheller.com",
  version := "1.1",
  scalaVersion := "2.12.0-M5"
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
