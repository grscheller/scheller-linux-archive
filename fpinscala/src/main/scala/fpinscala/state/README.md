## Functional design and combinator libraries

Chapter 6.

Implementing the State Monad while working through the exercises
in "Functional Programming in Scala" by Paul Chiusana and
Runar Bjarnason.

At this point in the book, I am not supposed to know the term
"monad" yet.  The code the book has gently led me to write something
very similar to the scalaz monad transformer.

### Package [fpinscala.rngStandalone](rngStandalone.scala)
* A random number generation package.
* Created while working through the book exercises.

### Program [rngStandaloneTest](exerciseCode/rngStandaloneTest.scala)
* Program to exercise package fpinscala.rngStandalone.

### Case class [fpinscala.state.State](State.scala#L3-L50)
* Abstracting out the State Monad from what was done in rngStandalone.scala.

### Companion object [fpinscala.state.State](State.scala#L52-L132)
* Abstracting out the State Monad from what was done in rngStandalone.scala.

### Package [fpinscala.state.rand](rand/)
* A reimplementation of package rngStandalone using fpinscala.state.State. 
* Rand class represents a "Random Variable" while "RNG" a value from
  the underlying probability domain space.
* Given a RNG, a Rand produces a definite value.
* Random variable as in probablity theory.

### Program [candyMachine](exerciseCode/candyMachine.scala)
* Standalone application to model a candy dispensing machine. 
* Based on FPinScala exercise 6.11.
