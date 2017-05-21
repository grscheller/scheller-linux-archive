## Purely functional parallelism

Chapter 7.

Exploring how to do concurrent processing in a purely functional way<br>
while working through the exercises in "Functional Programming in Scala"<br>
by Paul Chiusana and Runar Bjarnason.

### Package [`parallelism.javaFutures`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/parallelismJavaFutures.md)
The fpinscala.parallelism.javaFutures package internally uses, as well as<br>
returns, futures which adhere to the Java Futures API.  The futures produced<br>
are designed to be interoperative with java source code.  This implementation<br>
has deadlocking problems with fixed size threadpools and is quite a thread hog.

### Package [`parallelism`](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/parallelism/parallelism.md)
The fpinscala.parallelism package is the non-blocking, Actor based concurency<br>
API that the book gently leads you to develop.

