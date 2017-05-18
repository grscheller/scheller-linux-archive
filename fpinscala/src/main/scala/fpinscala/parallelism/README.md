## Purely functional parallelism

Chapter 7.

Exploring how to do concurrent processing in a purely functional way<br>
while working through the exercises in "Functional Programming in Scala"<br>
by Paul Chiusana and Runar Bjarnason.

### Trait [`Par[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L10-L89)
* Used to define future parallel computations and run them.

### Companon Object [`Par`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L91-L198)
* Utility object for the `Par[A]` trait.

### Private Case Class [`UnitFuture`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L200-L212)
* Wraps a value in a future.
* The future is basically born "done."
* This Future can be passed to Java code.

### Private Case Class [`Map2Future`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Par.scala#L214-L375)
* Future used by the Par.map2 method.
* Combines two parallel calculations with a function.
* Compatible with the java.util.concurrent.Future API.
* Some thread blocking issues.
* This Future can be passed to Java code.

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
