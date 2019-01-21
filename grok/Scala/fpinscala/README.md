## Functional Programming in Scala
I'm working my way through "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason to learn Scala.  Also, I am
teaching myself SBT using the online
[SBT reference manual](http://www.scala-sbt.org/).

I have set up my own build.sbt file and am not using the canned SBT
setup supplied [here](https://github.com/fpinscala/fpinscala) by the
authors.

### Packages and corresponding book chapters
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

### Typical work flow
Run code:
```
    $ sbt run
    $ sbt test:run
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
    > run
    > test:run
    > ~; compile; test:compile
    1. Waiting for source changes... (press enter to interrupt)
       .
       .
       .
    > run
```
repeat, while I edit files, use Unix commands like grep and
find, and perform GIT commands in other terminal windows.
