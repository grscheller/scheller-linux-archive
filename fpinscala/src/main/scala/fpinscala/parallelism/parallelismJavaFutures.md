## Classes/Traits of fpinscala.parallelism.javaFutures:

The fpinscala.parallelism.javaFutures package internally uses, as well as<br>
returns, futures which adhere to the Java Futures API.  The futures produced<br>
are designed to be interoperative with java source code.  This implementation<br>
has deadlocking problems with fixed size threadpools and is a thread hog.

### Trait [`parallelism.javaFutures.Par[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParallelismJavaFutures.scala#L30-L115)
* Used to define future parallel computations and run them.

### Companon Object [`parallelism.javaFutures.Par`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParallelismJavaFutures.scala#L117-L237)
* Utility methods for the `parallelism.javaFutures.Par[A]` trait.

### Private Case Class [`parallelism.javaFutures.UnitFuture`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParallelismJavaFutures.scala#L239-L251)
* Wraps a value in a future.
* The future is basically born "done."
* This Future can be passed to Java code.

### Private Case Class [`parallelism.javaFutures.Map2Future`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/ParallelismJavaFutures.scala#L253-L414)
* Future used by the Par.map2 method.
* Combines two parallel calculations with a function.
* Compatible with the java.util.concurrent.Future API.
* Some thread blocking issues.
* This Future can be passed to Java code.

##These are exercise packages fpinscala.parallelism.javaFutures:

### Program [ParTest1](exerciseCode/ParTest1.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParTest2](exerciseCode/ParTest2.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParTest3](exerciseCode/ParTest3.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParTest4](exerciseCode/ParTest4.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParJavaFutures4](exerciseCode/ParJavaFutures4.scala)
* Program to Par.flatMap

