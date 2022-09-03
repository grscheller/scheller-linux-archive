lazy val root = project
  .in(file("."))
  .settings(
      name := "scalaImplicits"
    , version := "0.1.5"
    , scalaVersion := "3.1.3"

    , scalacOptions += "-deprecation"
    , scalacOptions += "-feature"
    , scalacOptions += "-explain"
    , scalacOptions += "-explain-types"

    , libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % "test"
  )
