## Classes/Traits of fpinscala.parallelism.javaFutures:

The fpinscala.parallelism.javaFutures package internally uses, as well as
returns, futures which adhere to the Java Futures API.  The futures produced
are designed to be interoperative with java source code.  This implementation
has deadlocking problems with fixed size threadpools and is a thread hog.

### Trait [`parallelism.javaFutures.Par[A]`](parallelismJavaFutures.scala#L30-L115)
* Used to define future parallel computations and run them.

### Companon Object [`parallelism.javaFutures.Par`](parallelismJavaFutures.scala#L117-L240)
* Utility methods for the `parallelism.javaFutures.Par[A]` trait.

### Private Case Class [`parallelism.javaFutures.UnitFuture`](parallelismJavaFutures.scala#L242-L254)
* Wraps a value in a future.
* The future is basically born "done."
* This Future can be passed to Java code.

### Private Case Class [`parallelism.javaFutures.Map2Future`](parallelismJavaFutures.scala#L256-L421)
* Future used by the Par.map2 method.
* Combines two parallel calculations with a function.
* Compatible with the java.util.concurrent.Future API.
* Some thread blocking issues.
* This Future can be passed to Java code.

##These are exercise packages fpinscala.parallelism.javaFutures:

### Program [ParJavaFutures1](exerciseCode/ParJavaFutures1.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParJavaFutures2](exerciseCode/ParJavaFutures2.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParJavaFutures3](exerciseCode/ParJavaFutures3.scala)
* Program to exercise package fpinscala.parallelism.javaFutures

### Program [ParJavaFutures4](exerciseCode/ParJavaFutures4.scala)
* Program to Par.flatMap

## These programs test package fpinscala.parallelism.javaFutures:

### Program [parallelismTest](../../../../test/scala/fpinscala/parallelism/javaFutureCheck.scala)
* Property based testing for fpinscala.parallelism.javaFutures package.

### Program [parallelismTest](../../../../test/scala/fpinscala/parallelism/ijavaFutureParProp.scala)
* More property based testing for fpinscala.parallelism.javaFutures package.

