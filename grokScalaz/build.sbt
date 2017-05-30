lazy val commonSettings = Seq(
    version := "0.1-SNAPSHOT"
  , scalaVersion := "2.12.2"
  , exportJars := false
  , scalacOptions += "-deprecation"
  , libraryDependencies += "org.scalaz" %% "scalaz-core"       % "7.2.13"
  , libraryDependencies += "org.scalaz" %% "scalaz-effect"     % "7.2.13"
  , libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.13"
  , libraryDependencies += "org.scalaz" %% "scalaz-iteratee"   % "7.2.13")

lazy val root = (project in file("."))
  .aggregate(
       scalazREPL
     , parallelism)

lazy val scalazREPL = (project in file("scalazREPL"))
  .settings(
      commonSettings)

lazy val parallelism = (project in file("parallelism"))
  .settings(
      commonSettings)

