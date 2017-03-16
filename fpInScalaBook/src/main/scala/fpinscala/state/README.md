# Functional Programming in Scala Book Exercises

## Functional design and combinator libraries

Chapter 6.

Implementing the State Monad while working through the
exercises in  "Functional Programming in Scala" by Paul
Chiusana and Runar Bjarnason.

At this point in the book, I am not supposed to know the 
term "monad" yet.  The code the book has gently led me
to write something very similar to the scalaz monad transformer.

### Package fpinscala.rngStandalone -
    * A random number generation package created
      while working through the book exercises.
### src/test/scala/fpinscala/state/rngStandaloneTest.scala -
    * Program to exercise package fpinscala.rngStandalone.
### Package fpinscala.State -
    * Abstracting out the State Monad from what was done in rngStandalone.scala.
### Package fpinscala.rand -
    * A reimplementation of packagr rngStandalone using fpinscala.State. 
### src/test/scala/fpinscala/state/randTest.scala -
    * Program to exercise package fpinscala.rand.
### src/test/scala/fpinscala/state/candyMachines/candyMachines.scala -
    * Standalone application to model candy dispensing machines, 
      based on FPinScala exercise 6.11.  Useful to me as I grock
      the State monad.
