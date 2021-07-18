# Strictness and laziness

Chapter 5.

Implementing a Stream (Lazy List) data structure while working
through the exercises in  "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason.

## Trait [fpinscala.laziness.Stream](Stream.scala#L3-L307)

* implement a lazy list, or stream, using a trait and case classes

## Case Object [Empty](Stream.scala#L308)

* represents the unique empty stream
* subtype to all other Streams
* together with Cons, an example of an Algebraic Data Type (ADT)

## Case Class [Cons](Stream.scala#L309)

* represents a contravarient stream of type A
* together with Empty, an example of an Algebraic Data Type (ADT)

## Object [Stream](Stream.scala#L311-L415)

* companion object to the Stream trait
* contains "smart" constructors
* contains a variadic strict stream constuctor factory method (apply)

## Program [StreamTest](exerciseCode/StreamTest.scala)

* program that exercises fpinscala.laziness.Stream

## Program [infiniteStreamTest](exerciseCode/infiniteStreamTest.scala)

* using infinite streams with fpinscala.laziness.Stream

## Program [scalaInfiniteStreamTest](exerciseCode/scalaInfiniteStreamTest.scala)

* using infinite streams with standard scala libraries

## Program [scanRightTest](exerciseCode/scanRightTest.scala)

* test scanRight with fpinscala.laziness.Stream

## Program [hasSubsequenceStreamTest](exerciseCode/hasSubsequenceStreamTest.scala)

* test hasSubsequenceStreamTest with fpinscala.laziness.Stream

## Program [foldLeftTest](exerciseCode/foldLeftTest.scala)

* compare foldLeft vs foldRight with fpinscala.laziness.Stream

Exercises teaching the distinction between laziness and
non-strictness.  A function being lazy means it only evaluates
its arguments if there is a need.  This is sometimes refered to
by "call by need" or "called by name."  Most languages are strict,
they fully evaluate their arguments before they execute.  In C/C++
and Python, short circuit && and || are "functions" which are lazy
in their 2nd arguments.  A function f is strict if the expression
f(x) evaluates to bottom for all x that evaluate to bottom.

Scala is strict by default; it requires special syntax to be able
to "call by name."  Non-strictness allows right folds to manipulate
infinite data structures in almost metaphysical ways.  What stops
the recursion is this laziness/non-strictness.  It allows for very
efficient Stream compositions.

I have also added a withFilter method to enable guards in "for"
comprehensions in an efficient manner.  I modeled this after what
was done scala.Option.
