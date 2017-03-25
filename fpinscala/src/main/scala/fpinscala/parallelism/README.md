# Functional Programming in Scala Book Exercises

## Purely functional parallelism

Chapter 7.

Exploring how to do concurrent processing in a purely functional way.

### Type alias [`Par[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L11)
* Type alias to produce a future parallel computation.
* Defined in the Par Standalone (Utility) Object.

### Standalone Object [Par](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L6-L79)
* Utility object for the Par type alias.
* Provides a namespace for `Par[]` type alias and its related functions. 
