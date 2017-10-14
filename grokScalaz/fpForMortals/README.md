# FP for Mortals with Scalaz
Working through the book [Functional Programming for Mortals with Scalaz][1]
by Sam Halliday.

  [1]: http://leanpub.com/fpmortals "Functional Programming for Mortals"

## SBT Configuration used by book:
### The build.sbt file suggested by book:
```
   scalaVersion in ThisBuild := "2.12.3"
   scalaOptions in ThisBuild ++= Seq(
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
```

### Import section in code:
Use the following import statements in code:
```
   import scalaz._, Scalaz._
   import simulacrum._
   import stalactite._
```
## Projects:
### 1. Using [Scalaz from Scala REPL via SBT](scalazREPL/):
* Launch Scala REPL configured for Scalaz as above.
* First, cd to the scalazREPL directory, and then run
  ```
     $ sbt console
  ```
