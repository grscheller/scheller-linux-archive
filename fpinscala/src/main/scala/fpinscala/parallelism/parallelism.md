## Classes/Traits of fpinscala.parallelism:
The fpinscala.parallelism package is the non-blocking, Actor based concurency<br>
API that the book "Functional Programming in Scala" gently leads you to develop.

### Trait [`parallelism.Par[+A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Parallelism.scala#L18-L81)
* Used to define future parallel computations and run them.
* The `Par.run` method is the only way for client code to extract a value.
* Does not provide Future objects to clients as parallelism.javaFutures does.
* Use case is for client code to apply the blocking run method as last step.
* The run method is not intended to be used internally within the package.

### Companon Object [`parallelism.Par`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Parallelism.scala#L83-L182)
* Utility methods for the `parallelism.Par[A]` trait.

### Private Abstract Trait [`parallelism.ParFuture[+A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Parallelism.scala#L185-L191)
* A nonblocking "Future" which registers a callback for a parallel calculation.
* Has no get method, doesn't actually return anything at all.
* Actual return value is handled by an encapsulated side effect in run method.
* Not a Java Future at all, hence name change from book to avoid confusion.
* Use of the term "Future" may me 
* Made private[parallelism]

### Final Class [`Actor[A]`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Actor.scala#L20-L97)
* Processes messages of type A, one at a time.
* Messages are submitted to the actor with the ! method.
* Processing is typically performed asynchronously, the provided strategy.

####Memory consistency guarantee:
When each message is processed by the handler, any memory that it mutates is<br>
guaranteed to be visible by the handler when it processes the next message,<br>
even if the strategy runs the invocations of the handler on separate threads.<br>
This is achieved because the Actor reads a volatile memory location before<br>
entering its event loop, and writes to the same location before suspending.

### Companion Object [`Actor`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Actor.scala#L102-L109)
* Provides factory method to produce an actor from an ExecutorService.

### trait [`Strategy`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Actor.scala#L111-L120)
* When eventually instantiated, provides how to actually obtain a value.

### Compaion Object [`Strategy`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/Actor.scala#L122-L141)
* Provides two strategies
* First one using an java.util.concurrent.ExecutorService
* Strategy.fromExecutorService(es: ExecutiveService): Strategy
* Second one generates value in the existing thread
* Strategy.sequential(): Strategy

##These programs exercise package fpinscala.parallelism:

### Program [ExpParTest](exerciseCode/ExpParTest.scala)
* Create a fpinscala.parallelism.Par to calculate the exponential function.
* Purpose is to explore composing parallel calculations.
* Such a short calcultion not worth the thread overhead.

### Program [Sleepy](exerciseCode/Sleepy.scala)
* Program to exercise fpinscala.parallelism.Par.parMap
* Confirming no deadlocking issues
* Seeing effect of limiting thread number in threadpool.

