# Functional random number generator.

## Package fpinscala.rand

Implementing random number generation package rand
using the fpinscala.state.State Monad.  Basically,
reimplementating what was done in fpinscala.rngStandalone.

From chapter 6.

### Type alias [rand](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/state/rand/package.scala#L18-L19)
* Used to define the rand type alias at the rand package level.
* Using val Rand = fpinscala.state.State so the compiler can find the companion object.
* User code needs to use `new` key word to distinguish constructor from potential factory methods.
* Defined in the [rand](package.scala) package object.

### Class [RNG](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/state/rand/RNG.scala#L12-L17)
* Abstract base class for random number generators.

### Companion Object [RNG](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/state/rand/RNG.scala#L19-L101)
* Companion object for for RNG and its subclasses.

### Class [LCG](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/state/rand/RNG.scala#L103-L143)
* Linear Congruence Generator implementation for RNG.

### Program [randTest](randTest.scala)
* Program to exercise package fpinscala.rand.
