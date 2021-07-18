# Functional Programming in Scala

I'm working my way through "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason to learn Scala.  Also, I am
teaching myself SBT using the online
[SBT reference manual](http://www.scala-sbt.org/).

I have set up my own build.sbt file and am not using the canned SBT
setup supplied [here](https://github.com/fpinscala/fpinscala) by the
authors.

Scala is a hybrid functional/oo language which runs on
the Java Virtual Machine (JVM).

## Packages and corresponding book chapters

* Chapter 2: [Getting started with functional programming in Scala](src/main/scala/fpinscala/gettingstarted/)
* Chapter 3: [Functional data structures](src/main/scala/fpinscala/datastructures/)
* Chapter 4: [Handling errors without exceptions](src/main/scala/fpinscala/errorhandling/)
* Chapter 5: [Strictness and laziness](src/main/scala/fpinscala/laziness/)
* Chapter 6: [Purely functional state](src/main/scala/fpinscala/state/)
* Chapter 7: [Purely functional parallelism](src/main/scala/fpinscala/parallelism/)
* Chapter 8: [Property-based testing](src/main/scala/fpinscala/testing/)
* Chapter 9: [Parser combinators](src/main/scala/fpinscala/parsing/)
* Chapter 10: Monoids
* Chapter 11: Monads
* Chapter 12: Applicative and traversable functors
* Chapter 13: External effects and I/O
* Chapter 14: Local effects and mutable state
* Chapter 15: Stream processing and incremental I/O

## Typical work flow

Run code:

```
    $ sbt run
    $ sbt test/run
```

My usual work flow is "continuous compile,"

```
    $ sbt
    [info] welcome to sbt 1.5.4 (Oracle Corporation Java 11.0.11)
    [info] loading settings for project fpinscala-build-build from metals.sbt ...
    [info] loading project definition from /home/grs/devel/scheller-linux-archive/grok/Scala2/fpinscala/project/project
    [info] loading settings for project fpinscala-build from metals.sbt ...
    [info] loading project definition from /home/grs/devel/scheller-linux-archive/grok/Scala2/fpinscala/project
    [success] Generated .bloop/fpinscala-build.json
    [success] Total time: 0 s, completed Jul 17, 2021, 6:13:59 PM
    [info] loading settings for project root from build.sbt ...
    [info] set current project to fpinscala (in build file:/home/grs/devel/scheller-linux-archive/grok/Scala2/fpinscala/)
    [info] sbt server started at local:///home/grs/.sbt/1.0/server/c7bf9eed92a4e47cd0cb/sock
    [info] started sbt server
    sbt:fpinscala>
    sbt:fpinscala> ~; compile; Test / compile
    [success] Total time: 0 s, completed Jul 17, 2021, 6:02:51 PM
    [success] Total time: 0 s, completed Jul 17, 2021, 6:02:52 PM
    [info] 1. Monitoring source files for root/compile; root/Test / compile...
    [info]    Press <enter>  to interrupt or '?' for more options.
       .
       .
       .
    [info] Received input event: CancelWatch.
    sbt:fpinscala> run
       .
       .
       .
    sbt:fpinscala> Test / run
       .
       .
       .
    sbt:fpinscala> ~; compile; test:compile
    1. Waiting for source changes... (press enter to interrupt)
       .
       .
       .
    [info] Received input event: CancelWatch.
    sbt:fpinscala> run
       .
       .
       .
    sbt:fpinscala> ~; compile; Test / compile
       .
       .
       .
```

Repeat, while I edit files, use Unix commands like grep and
find, and perform GIT commands in other terminal windows.
