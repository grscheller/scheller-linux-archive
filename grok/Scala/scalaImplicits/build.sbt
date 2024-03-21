lazy val root = project
  .in(file("."))
  .settings(
      name := "scalaImplicits"
    , version := "0.2.0"
    , scalaVersion := "3.4.0"

    , scalacOptions += "-deprecation"
    , scalacOptions += "-feature"
    , scalacOptions += "-explain"
    , scalacOptions += "-explain-types"

    , libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test"
  )
