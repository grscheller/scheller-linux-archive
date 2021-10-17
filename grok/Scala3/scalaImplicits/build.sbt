lazy val root = project
  .in(file("."))
  .settings(
      name := "scalaImplicits"
    , version := "0.1.1"
    , scalaVersion := "3.0.2"

    , scalacOptions += "-deprecation"
    , scalacOptions += "-feature"
    , scalacOptions += "-explain"
    , scalacOptions += "-explain-types"

    , libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
