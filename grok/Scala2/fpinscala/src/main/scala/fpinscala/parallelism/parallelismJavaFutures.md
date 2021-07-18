# Classes/Traits of fpinscala.parallelism.javaFutures

The fpinscala.parallelism.javaFutures package internally uses, as well as
returns, futures which adhere to the Java Futures API.  The futures produced
are designed to be interoperative with java source code.  This implementation
has deadlocking problems with fixed size threadpools and is a thread hog.

## Trait [parallelism.javaFutures.Par[A]](parallelismJavaFutures.scala#L30-L105)

* used to define future parallel computations and run them

## Companon Object [parallelism.javaFutures.Par](parallelismJavaFutures.scala#L107-L226)

* utility methods for the `parallelism.javaFutures.Par[A]` trait

## Private Case Class [parallelism.javaFutures.UnitFuture](parallelismJavaFutures.scala#L228-L239)

* wraps a value in a future
* the future is basically born "done"
* this Future can be passed to Java code

## Private Case Class [parallelism.javaFutures.Map2Future](parallelismJavaFutures.scala#L241-L401)

* future used by the Par.map2 method
* combines two parallel calculations with a function
* compatible with the java.util.concurrent.Future API
* some thread blocking issues
* this Future can be passed to Java code

## These exercise package fpinscala.parallelism.javaFutures

### Program [ParJavaFutures1](exerciseCode/ParJavaFutures1.scala)

* exercises package fpinscala.parallelism.javaFutures

### Program [ParJavaFutures2](exerciseCode/ParJavaFutures2.scala)

* exercises package fpinscala.parallelism.javaFutures

### Program [ParJavaFutures3](exerciseCode/ParJavaFutures3.scala)

* exercises package fpinscala.parallelism.javaFutures

### Program [ParJavaFutures4](exerciseCode/ParJavaFutures4.scala)

* exercises`Par.flatMap`

## These programs test package fpinscala.parallelism.javaFutures

### Program [parallelismTest](../../../../test/scala/fpinscala/parallelism/javaFutureCheck.scala)

* property based testing for fpinscala.parallelism.javaFutures package

### Program [parallelismTest](../../../../test/scala/fpinscala/parallelism/javaFutureParProp.scala)

* more property based testing for fpinscala.parallelism.javaFutures package
