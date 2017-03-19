# Functional Programming in Scala Book Exercises

## Strictness and laziness

Chapter 5.

Implementing a Stream (Lazy List) data structure while working<br>
through the exercises in  "Functional Programming in Scala"<br>
by Paul Chiusana and Runar Bjarnason.

### Trait [fpinscala.laziness.Stream](Stream.scala)
* Implement a lazy list, or stream, using a trait and case classes.

### Program [StreamTest](StreamTest.scala)
* A program that exercises fpinscala.laziness.Stream.

### Program [infiniteStreamTest](infiniteStreamTest.scala)
* Using infinite streams with fpinscala.laziness.Stream.

### Program [scalaInfiniteStreamTest](scalaInfiniteStreamTest.scala)
* Using infinite streams with standard scala libraries.

### Program [scanRightTest](scanRightTest.scala)
* Test scanRight with fpinscala.laziness.Stream.

### Program [hasSubsequenceStreamTest](hasSubsequenceStreamTest.scala)
* Test hasSubsequenceStreamTest with fpinscala.laziness.Stream.

### Program [foldLeftTest](foldLeftTest.scala)
* Compare foldLeft vs foldRight with fpinscala.laziness.Stream.

Exercises teaching the distinction between laziness and<br>
non-strictness.  A function being lazy means it only evaluates<br>
its arguments if there is a need.  This is sometimes refered to<br>
by "call by need" or "called by name."  Most languages are strict,<br>
they fully evaluate their arguments before they execute.  In C/C++<br>
and Python, short circuit && and || are "functions" which are lazy<br>
in their 2nd arguments.  A function f is strict if the expression<br>
f(x) evaluates to bottom for all x that evaluate to bottom.

Scala is strict by default; it requires special syntax to to<br>
"call by name."  Non-strictness allows right folds to manipulte<br>
infinite data structures in almost metaphysical ways.  What stops<br>
the recursion is this laziness/non-strictness.  It allows for very<br>
efficient Stream compositions.

I have also added a withFilter method to enable guards in "for"<br>
comprehensions in an efficient manner.  I modeled this after what<br>
was done scala.Option.
