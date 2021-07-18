# Classes/Traits of fpinscala.parallelism

The fpinscala.parallelism package is the non-blocking, Actor based concurency
API that the book "Functional Programming in Scala" gently leads you to develop.
My version includes error handling.

## Trait [parallelism.Par[+A]](parallelism.scala#L27-L173)

* used to define future parallel computations and run them
* the `Par.run` method is the only way for client code to extract a value
* does not provide Future objects to clients as parallelism.javaFutures does
* use case is for client code to apply the blocking run method as last step
* run method not intended to be used internally within the package

## Companon Object [parallelism.Par](parallelism.scala#L176-L356)

* utility methods for the `parallelism.Par[A]` trait

## Private Abstract Trait [parallelism.ParFuture[+A]](parallelism.scala#L14-L25)

* nonblocking "Future" which registers a callback for a parallel calculation
* has no get method, doesn't actually return anything at all
* actual return value is handled by an encapsulated side effect in run method
* not a Java Future, hence I changed name from book to avoid confusion
* use of the term "Future" maybe in line with various libraries

## Final Class [Actor[A]](Actor.scala#L19-L98)

* processes messages of type A, one at a time
* messages are submitted to the actor with the `!` method
* processing is typically performed asynchronously, the provided strategy

**Memory consistency guarantee:** When each message is processed by the
handler, any memory that it mutates is guaranteed to be visible by the
handler when it processes the next message, even if the strategy runs
the invocations of the handler on separate threads.  This is achieved
because the Actor reads a volatile memory location before entering its
event loop, and writes to the same location before suspending.

## Companion Object [Actor](Actor.scala#L100-L107)

* provides factory method to produce an actor from an ExecutorService

## trait [Strategy](Actor.scala#L109-L117)

* when eventually instantiated, provides how to actually obtain a value

## Compaion Object [Strategy](Actor.scala#L119-L138)

* provides two strategies
* first one using an java.util.concurrent.ExecutorService
  * strategy.fromExecutorService(es: ExecutiveService): Strategy
* second one generates value in the existing thread
  * strategy.sequential(): Strategy

## Singleton object [ParProp](ParProp.scala#L12-L39)

* for use with fpinscala.testing package

## These programs exercise package fpinscala.parallelism

### Program [ExpParTest](exerciseCode/ExpParTest.scala)

* create an fpinscala.parallelism.Par to calculate the exponential function
* purpose is to explore composing parallel calculations
* such a short calcultion not worth the thread overhead

### Program [Sleepy](exerciseCode/Sleepy.scala)

* exercises fpinscala.parallelism.Par.parMap
* confirming there are no deadlocking issues
* exploring how limiting thread number affects the thread pool

### Program [ParParFutures1](exerciseCode/ParParFutures1.scala)

* exercises Par.flatMap, with error handling

### Program [ParParFutures2](exerciseCode/ParParFutures2.scala)

* exercises package fpinscala.parallelism

### Program [ParParFutures3](exerciseCode/ParParFutures3.scala)

* exercises package fpinscala.parallelism

## These programs test package fpinscala.parallelism

### Program [parallelismTest](../../../../test/scala/fpinscala/parallelism/parallelismCheck.scala)

* property based testing for fpinscala.parallelism package

### Program [parallelismTest](../../../../test/scala/fpinscala/parallelism/parallelismParProp.scala)

* more property based testing for fpinscala.parallelism package
