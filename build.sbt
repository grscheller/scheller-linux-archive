lazy val commonSettings = Seq(
  organization := "mil.afrl.dtic",
  version := "1.0",
  scalaVersion := "2.12.0-M4"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*)

/* If true, this will cause the jar file to be used by run,
   test, console, and other tasks that use the full classpath.

   There is an sbt plug-in called sbt-assembly
   that will generate a fat jar with everthing including
   the kitchen sink that can be plopped anywhere where there
   is a jvm and run.

*/
exportJars := false

// Build to require/target java8

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")
scalacOptions += "-target:jvm-1.8"

/* To force the use of java8 libraries only */
initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8")
    sys.error("Java 8 is required for this project.")
}
