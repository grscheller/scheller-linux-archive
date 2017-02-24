I am working my way through the book "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason to learn Scala.  Using the online
reference manual at http://www.scala-sbt.org/ to learn sbt.

I am currently working on chapter 6.

Libraries are under "src/main", test objects are under "src/test".
These tests just exercise the libraries I write.

Run code:
  ```
    $ sbt test:run
    $ sbt run
  ```
The versions of sbt, scala, and java I am currently using:
  ```
    $ sbt sbtVersion
    [info] Set current project to root (in build file:/home/geoff/devel/learn/learnScala/fpInScalaBook/)
    [info] 0.13.13

    $ sbt console
    [info] Set current project to root (in build file:/home/geoff/devel/learn/learnScala/fpInScalaBook/)
    [info] Starting scala interpreter...
    [info] 
    Welcome to Scala 2.12.1 (OpenJDK 64-Bit Server VM, Java 1.8.0_121).
    Type in expressions for evaluation. Or try :help.

    scala> 
  ```
My usual work flow is "continuous compile":
  ```
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
    > test:run
  ```
repeat, while I edit files, use Unix commands like grep and find,
and perform GIT commands in other terminal windows.
