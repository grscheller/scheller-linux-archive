lazy val commonSettings = Seq(
    version := "0.1-SNAPSHOT"
  , scalaVersion := "2.12.2"
  , exportJars := false
  , scalacOptions += "-deprecation"
  , libraryDependencies += "org.scalaz" %% "scalaz-core"       % "7.2.12"
  , libraryDependencies += "org.scalaz" %% "scalaz-effect"     % "7.2.12"
  , libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.12"
  , libraryDependencies += "org.scalaz" %% "scalaz-iteratee"   % "7.2.12")

lazy val root = (project in file("."))
  .aggregate(
       scalazREPL
     , foobar)

lazy val scalazREPL = (project in file("scalazREPL"))
  .settings(
      commonSettings)

lazy val foobar = (project in file("foobar"))
  .settings(
      commonSettings)

