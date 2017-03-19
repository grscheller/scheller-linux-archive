# Functional Programming in Scala Book Exercises

## Functional design and combinator libraries

Chapter 6.

Implementing the State Monad while working through the exercises<br>
in "Functional Programming in Scala" by Paul Chiusana and
Runar Bjarnason.

At this point in the book, I am not supposed to know the term<br>
"monad" yet.  The code the book has gently led me to write something<br>
very similar to the scalaz monad transformer.

### Package [fpinscala.rngStandalone](rngStandalone.scala)
* A random number generation package created
  while working through the book exercises.

### src/test/scala/fpinscala/state/rngStandaloneTest.scala
* Program to exercise package fpinscala.rngStandalone.

### case class [fpinscala.state.State](State.scala)
* Abstracting out the State Monad from
  what was done in rngStandalone.scala.

### Package [fpinscala.state.rand](rand/)
* A reimplementation of package rngStandalone
  using fpinscala.state.State. 

### src/test/scala/fpinscala/state/randTest.scala
* Program to exercise package fpinscala.rand.

### src/test/scala/fpinscala/state/candyMachines/candyMachines.scala
* Standalone application to model candy dispensing machines, 

* Based on FPinScala exercise 6.11.  Using this
  as I grock the State monad.
