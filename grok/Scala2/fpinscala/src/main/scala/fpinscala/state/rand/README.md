# Functional random number generator

Implementing random number generation package rand
using the fpinscala.state.State Monad.  Basically,
reimplementating what was done in fpinscala.rngStandalone.

From chapter 6.

## Abstract Class [RNG](RNG.scala#L3-L9)

* abstract base class for random number generators
* represents an underlying probability space for random variables
* random variables implemented with Rand class

## Case Class [LCG](RNG.scala#L11-L51)

* linear Congruence Generator implementation for RNG
* uses the same algoritm as java.util.Random and glibc

## Case Class [Rand](Rand.scala#L5-L31)

* random variable as in probability theory
* not a "function" which produces a different "random" value whenever called
* the`RNG`Class generates values from the underlying probability space

## Companion Object [Rand](Rand.scala#L33-L289)

* contains utility functions for the Rand case class
* contains various probability distributions
* contains functions to create joint probaility distributions

## Program [randTest](../exerciseCode/randTest.scala)

* exercises package fpinscala.rand
