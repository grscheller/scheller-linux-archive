lazy val commonSettings = Seq(
  organization := "mil.afrl.dtic",
  version := "1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*)

/* This will cause the jar file to be used by run, test, 
   console, and other tasks that use the full classpath.

   I commented it out since its best use case is for a
   single standalong, not necessarily totally self contained,
   application, where I really want to run/test everything
   from the jar files.  

   There is an sbt plug-in called sbt-assembly
   that will generate a fat jar with everthing including
   the kitchen sink that can be plopped anywhere where there
   is a jvm and run.

*/
exportJars := false

/* An attempt to get at the classpath to try and run
   scala directly.  Didn't work.
*/
//lazy val printClasspath = task {
//  this.runClasspath.getPaths.foreach(println)
//  None
//}
