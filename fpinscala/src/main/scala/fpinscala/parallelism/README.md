## Purely functional parallelism

Chapter 7.

Exploring how to do concurrent processing in a purely functional way<br>
while working through the exercises in "Functional Programming in Scala"<br>
by Paul Chiusana and Runar Bjarnason.

### Type alias [`Par[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L27)
* Type alias to produce a future parallel computation.
* Defined in the `Par` Standalone (Utility) Object.

### Standalone Object [`Par`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L24-L368)
* Utility object for the `Par[A]` type alias.
* Provides a namespace for `Par[A]` type alias and its related functions. 

### Private Case Class [`UnitFuture`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L194-L206)
* Wraps a value in a Future.
* Future is basically born "done."

### Private Case Class [`Map2Future`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L208-L367)
* Future used by the Par.map2 method.
* Combines two parallel calculations with a function.
* Compatible with the java.util.concurrent.Future API.
* This Future can be passed to Java code.
* Some thread blocking issues.

### Program [ParTest1](exerciseCode/ParTest1.scala)
* Program to exercise fpinscala.parallelism.Par 

### Program [ParTest2](exerciseCode/ParTest2.scala)
* Program to exercise fpinscala.parallelism.Par 

### Program [ParTest3](exerciseCode/ParTest3.scala)
* Program to exercise fpinscala.parallelism.Par

### Program [ExpParTest](exerciseCode/ExpParTest.scala)
* Program to create a Par to calculate the exponential function.
* Purpose is to explore composing parallel calculations.

### Program [Sleepy](exerciseCode/Sleepy.scala)
* Program to exercise fpinscala.parallelism.Par.parMap
* Seeing effect of limiting thread number without deadlocking.
