val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
      name := "scalaImplicits"
    , version := "0.1.0"
    , scalaVersion := scala3Version

    , scalacOptions += "-deprecation"
    , scalacOptions += "-feature"
    , scalacOptions += "-explain"
    , scalacOptions += "-explain-types"

    , libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
