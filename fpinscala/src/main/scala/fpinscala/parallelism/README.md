## Purely functional parallelism

Chapter 7.

Exploring how to do concurrent processing in a purely functional way<br>
while working through the exercises in "Functional Programming in Scala"<br>
by Paul Chiusana and Runar Bjarnason.

### Type alias [`Par[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala)
* Type alias to produce a future parallel computation.
* Defined in the `Par` Standalone (Utility) Object.

### Standalone Object [`Par`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala)
* Utility object for the `Par[A]` type alias.
* Provides a namespace for `Par[A]` type alias and its related functions. 

### Program [ParTest](ParTest.scala)
* Program to exercise fpinscala.parallelism.Par. 

### Program [ExpParTest](ExpParTest.scala)
* Program to create a Par to calculate the exponential function.
* It is quite quick, despite me not trying to optimize it at all.
* Actually, it only ever uses one thread from the thread pool at any one time.
