lazy val root = project
  .in(file("."))
  .settings(
      name := "ADT"
    , version := "0.1.0"
    , scalaVersion := "3.1.0"

    , scalacOptions += "-deprecation"
    , scalacOptions += "-feature"
    , scalacOptions += "-explain"
    , scalacOptions += "-explain-types"

  )
