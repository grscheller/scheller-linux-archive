// To produces target/(project)-assembly-(version).jar file,
// in sbt use:
//   > assembly
import AssemblyKeys._  // put this at the top of the file
assemblySettings

// To collect all dependent jars into target/pack folder,
// in sbt use
//   > pack
packSettings
// [Optional: Mappings from a program name to the corresponding Main class ]
// packMain := Map("hello" -> "myprog.Hello")

// Project name (artifact name in Maven)
name := "FormatedDate"

// orgnization name (e.g., the package name of the project)
organization := "com.scheller.geoffrey"

version := "1.0-SNAPSHOT"

// project description
description := "Print local formated date"

// Enables publishing to maven repo
publishMavenStyle := true

// Do not append Scala versions to the generated artifacts
crossPaths := false

// This forbids including Scala related libraries into the dependency
autoScalaLibrary := false

// library dependencies. (orginization name) % (project name) % (version)
libraryDependencies ++= Seq(
   "org.apache.commons" % "commons-math3" % "3.6.1",
   "org.fluentd" % "fluent-logger" % "0.2.10",
   "org.mockito" % "mockito-core" % "1.9.5" % "test"  // Test-only dependency
)

