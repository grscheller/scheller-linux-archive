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

### Standalone Object [`ParUtils`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParUtils.scala)
* Par related functions of a less general nature.

### Program [ParTest1](exerciseCode/ParTest1.scala)
### Program [ParTest1](exerciseCode/ParTest1.scala)
* Program to exercise fpinscala.parallelism.Par 

### Program [ParTest2](exerciseCode/ParTest2.scala)
* Program to exercise fpinscala.parallelism.Par 

### Program [ParTest3](exerciseCode/ParTest3.scala)
* Program to exercise fpinscala.parallelism.{Par,ParUtils}

### Program [ExpParTest](exerciseCode/ExpParTest.scala)
* Program to create a Par to calculate the exponential function.
* Purpose is to explore composing parallel calculations.
