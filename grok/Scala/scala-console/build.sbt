lazy val root = project
  .in(file("."))
  .settings(
      name := "scala-console"
    , version := "0.2.0"
    , scalaVersion := "3.3.3"

    , scalacOptions += "-deprecation"
    , scalacOptions += "-feature"
    , scalacOptions += "-explain"
    , scalacOptions += "-explain-types"
  )
