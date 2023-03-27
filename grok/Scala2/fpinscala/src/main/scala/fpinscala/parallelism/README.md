# Purely functional parallelism

Chapter 7.

Exploring how to do concurrent processing in a purely functional way
while working through the exercises in "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason.

## Package [`parallelism.javaFutures`](parallelismJavaFutures.md)

The fpinscala.parallelism.javaFutures package internally uses, as well as
returns, futures which adhere to the Java Futures API.  The futures produced
are designed to be interoperative with java source code.  This implementation
has deadlocking problems with fixed size threadpools and is quite a thread hog.

## Package [`parallelism`](parallelism.md)

The fpinscala.parallelism package is the non-blocking, Actor based concurency
API that the book gently leads you to develop.
