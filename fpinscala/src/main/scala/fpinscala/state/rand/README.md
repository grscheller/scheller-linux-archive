# Functional Programming in Scala Book Exercises

## Functional random number generator.

Implementing random number generation package rand
using the fpinscala.state.State Monad.  Basically,
reimplementating what was done in fpinscala.rngStandalone.

From chapter 6.

### Class [RNG](https://github.com/grscheller/scheller-linux-archive/blob/d41fbbba704573d44c0c9821554915fb134af584/fpinscala/src/main/scala/fpinscala/state/rand/RNG.scala#L15-L17)
* Abstract base class for random number generators.

### Class [LCG](https://github.com/grscheller/scheller-linux-archive/blob/d41fbbba704573d44c0c9821554915fb134af584/fpinscala/src/main/scala/fpinscala/state/rand/RNG.scala#L131-L143)
* Linear Congruence Generator implementation for RNG.

### [fpinscala.state.rand package object](package.scala)
* Used to define the rand type alias at the rand package level.

### Program [randTest](randTest.scala)
* Program to exercise package fpinscala.rand.

