lazy val root = project
  .in(file("."))
  .settings(
      name := "scalaImplicits"
    , version := "0.1.6"
    , scalaVersion := "3.2.0"

    , scalacOptions += "-deprecation"
    , scalacOptions += "-feature"
    , scalacOptions += "-explain"
    , scalacOptions += "-explain-types"

    , libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % "test"
  )
