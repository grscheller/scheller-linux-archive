I am working my way through the book "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason to learn Scala.  Using the online
reference manual at http://www.scala-sbt.org/ to learn sbt.

So far I have almost completed chapter 5.

I am doing this in a another (local) GIT repo.  As I make progress,
I will periodically make snapshots for my public repo.

Libraries are under "src", test scripts (except for chapter 2 stuff)
are under "test".  These tests just exercise the libraries.

Run code:
  $ sbt test:run
  $ sbt run

The version of sbt I am currently using: 0.13.13
The version of scala and java are:

  $ sbt sbtVersion
  [info] Set current project to root (in build file:/home/geoff/devel/learn/learnScala/fpInScalaBook/)
  [info] 0.13.13

  $ sbt console
  [info] Set current project to root (in build file:/home/geoff/devel/learn/learnScala/fpInScalaBook/)
  [info] Starting scala interpreter...
  [info] 
  Welcome to Scala 2.12.0-RC2 (OpenJDK 64-Bit Server VM, Java 1.8.0_112).
  Type in expressions for evaluation. Or try :help.

  scala> 

My usual work flow is "continuous compile":

  $ sbt
  > ~; compile; test:compile
  [success] Total time: 0 s, completed Oct 17, 2016 2:31:19 PM
  [success] Total time: 0 s, completed Oct 17, 2016 2:31:19 PM
  1. Waiting for source changes... (press enter to interrupt)
     .
     .
     .
  > test:run
  > ~; compile; test:compile
  1. Waiting for source changes... (press enter to interrupt)
     .
     .
     .

repeat, while I edit files, use Unix commands like grep and find,
 and perform GIT commands in other terminal windows.
