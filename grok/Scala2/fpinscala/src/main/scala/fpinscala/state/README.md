# Functional design and combinator libraries

Chapter 6.

Implementing the State Monad while working through the exercises
in "Functional Programming in Scala" by Paul Chiusana and
Runar Bjarnason.

At this point in the book, I am not supposed to know the term
"monad" yet.  The code the book has gently led me to write something
very similar to the scalaz monad transformer.

## Package [fpinscala.rngStandalone](rngStandalone.scala)

* random number generation package
* created while working through the book exercises

## Program [rngStandaloneTest](exerciseCode/rngStandaloneTest.scala)

* exercises package fpinscala.rngStandalone

## Case Class [fpinscala.state.State](State.scala#L3-L50)

* abstracting out the State Monad from what was done in rngStandalone.scala

## Companion Object [fpinscala.state.State](State.scala#L52-L127)

* abstracting out the State Monad from what was done in rngStandalone.scala

## Package [fpinscala.state.rand](rand/)

* a reimplementation of package rngStandalone using fpinscala.state.State
* `Rand`class objects represent "Random Variables"
* `RNG`class objects generate values from underlying probability domain space
* given a RNG, a Rand produces a definite value
* random variable as in probablity theory

## Program [candyMachine](exerciseCode/candyMachine.scala)

* standalone application to model a candy dispensing machine
* based on FPinScala exercise 6.11
