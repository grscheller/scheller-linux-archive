## Functional Programming in Scala Book Exercises

I'm working my way through "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason to learn Scala.  Also, I am
teaching myself SBT using the online
[SBT reference manual](http://www.scala-sbt.org/).

I have set up my own build.sbt file and am not using the canned SBT
setup supplied [here](https://github.com/fpinscala/fpinscala) by the
authors.

### Packages and corresponding book chapters

#### [fpinscala.gettingstarted](src/main/scala/fpinscala/gettingstarted/)
* Chapter 2: Getting started with functional programming in Scala

#### [fpinscala.datastructures](src/main/scala/fpinscala/datastructures/)
* Chapter 3: Functional data structures

#### [fpinscala.errorhandling](src/main/scala/fpinscala/errorhandling/)
* Chapter 4: Handling errors without exceptions

#### [fpinscala.laziness](src/main/scala/fpinscala/laziness/)
* Chapter 5: Strictness and laziness

#### [fpinscala.state](src/main/scala/fpinscala/state/)
* Chapter 6: Purely functional state

#### [fpinscala.parallelism](src/main/scala/fpinscala/parallelism/)
* Chapter 7: Purely functional parallelism

#### [fpinscala.testing](src/main/scala/fpinscala/testing/)
* Chapter 8: Property-based testing

#### [fpinscala.parsing](src/main/scala/fpinscala/parsing/)
* Chapter 9: Parser combinators

#### fpinscala.monoids
* Chapter 10: Monoids

#### fpinscala.monads
* Chapter 11: Monads

#### fpinscala.applicative
* Chapter 12: Applicative and traversable functors

#### fpinscala.iomonad
* Chapter 13: External effects and I/O

#### fpinscala.localeffects
* Chapter 14: Local effects and mutable state

#### fpinscala.streamingio
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