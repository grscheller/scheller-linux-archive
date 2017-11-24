scalaVersion in ThisBuild := "2.12.4"
scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum"  % "0.11.0",
  "com.chuusai"          %% "shapeless"   % "2.3.2" ,
  "com.fommil"           %% "stalactite"  % "0.0.5" ,
  "org.scalaz"           %% "scalaz-core" % "7.2.15"
) 

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
)

initialCommands in console := """
  import scalaz._
  import Scalaz._
  import simulacrum._
  import stalactite._
"""
