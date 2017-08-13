## Functional random number generator

Implementing random number generation package rand
using the fpinscala.state.State Monad.  Basically,
reimplementating what was done in fpinscala.rngStandalone.

From chapter 6.

### Abstract Class [RNG](RNG.scala#L7-L12)
* Abstract base class for random number generators.
* Represents an underlying probability space for random variables.
* Random variables implemented with Rand class.

### Case Class [LCG](RNG.scala#L14-L54)
* Linear Congruence Generator implementation for RNG.
* Uses the same algoritm as java.util.Random and glibc.

### Case Class [Rand](Rand.scala#L5-L32)
* Random variable as in probability theory.
* Not a "function" which produces a different "random" value whenever called.
* The RNG Class represents the underlying probability space.

### Companion Object [Rand](Rand.scala#L34-L264)
* Contains utility functions for the Rand case class.
* Contains various probability distributions.
* Contains functions to create joint probaility distributions.

### Program [randTest](../exerciseCode/randTest.scala)
* Program to exercise package fpinscala.rand.
