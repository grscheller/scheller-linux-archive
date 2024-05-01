lazy val root = project
  .in(file("."))
  .settings(
      name := "ADT"
    , version := "0.1.0"
    , scalaVersion := "3.3.3"

    , scalacOptions += "-deprecation"
    , scalacOptions += "-feature"
    , scalacOptions += "-explain"
    , scalacOptions += "-explain-types"

  )
