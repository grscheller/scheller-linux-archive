## Purely functional parallelism

Chapter 7.

Exploring how to do concurrent processing in a purely functional way<br>
while working through the exercises in "Functional Programming in Scala"<br>
by Paul Chiusana and Runar Bjarnason.

The fpinscala.parallelism.javaFutures package internally uses, as well as<br>
returns, futures which adhere to the Java Futures API.  The futures produced<br>
are designed to be interoperative with java source source code.

These are the classes/traits of fpinscala.parallelism.javaFutures:

### Trait [`parallelism.javaFutures.Par[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParallelismJavaFutures.scala#L29-L108)
* Used to define future parallel computations and run them.

### Companon Object [`parallelism.javaFutures.Par`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParallelismJavaFutures.scala#L110-L217)
* Utility methods for the `parallelism.javaFutures.Par[A]` trait.

### Private Case Class [`parallelism.javaFutures.UnitFuture`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParallelismJavaFutures.scala#L219-L231)
* Wraps a value in a future.
* The future is basically born "done."
* This Future can be passed to Java code.

### Private Case Class [`parallelism.javaFutures.Map2Future`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParallelismJavaFutures.scala#L233-L394)
* Future used by the Par.map2 method.
* Combines two parallel calculations with a function.
* Compatible with the java.util.concurrent.Future API.
* Some thread blocking issues.
* This Future can be passed to Java code.

These are the classes/traits of fpinscala.parallelism:

### Trait [`parallelism.Par[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Parallelism.scala#L18-L81)
* Used to define future parallel computations and run them.
* The `Par.run` method is the only way for client code to extract a value.
* No longer provides Future objects to clients.
* Use case is for client code to apply the blocking run method as last step.
* The run method is not intended to be used internally.

### Companon Object [`parallelism.Par`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Parallelism.scala#L83-L182)
* Utility methods for the `parallelism.Par[A]` trait.

### Trait [`parallelism.ParFuture[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Parallelism.scala#L185-L191)
* An early step in developing a nonblocking version of a future.
* Extends java.util.concurrent.Future with an privite[parallelism] apply method.
* Made private[parallelism]

### Private Case Class [`parallelism.UnitFuture`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Parallelism.scala#L193-L204)
* Wraps a value in a ParFuture.
* The future is basically born "done."
* For now, the same as is done for parallism.javaFutures version.

### Private Case Class [`parallelism.Map2Future`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Parallelism.scala#L206-L232)
* ParFuture used by the Par.map2 method.
* Combines two parallel calculations with a function.
* Some thread blocking issues.
* For now, stripped down version of what is done in parallism.javaFutures.

These are exercide packages fpinscala.parallelism and
fpinscala.parallelism.javaFutures:

### Program [ParTest1](exerciseCode/ParTest1.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParTest2](exerciseCode/ParTest2.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParTest3](exerciseCode/ParTest3.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ExpParTest](exerciseCode/ExpParTest.scala)
* Create a fpinscala.parallelism.Par to calculate the exponential function.
* Purpose is to explore composing parallel calculations.
* Such a short calcultion not worht the thread overhead.

### Program [Sleepy](exerciseCode/Sleepy.scala)
* Program to exercise fpinscala.parallelism.Par.parMap
* Seeing effect of limiting thread number without deadlocking.
