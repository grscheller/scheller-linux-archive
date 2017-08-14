## Strictness and laziness

Chapter 5.

Implementing a Stream (Lazy List) data structure while working
through the exercises in  "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason.

### Trait [fpinscala.laziness.Stream](Stream.scala#L4-L310)
* Implement a lazy list, or stream, using a trait and case classes.

### Case Object [Empty](Stream.scala#L311)
* Represents the unique empty stream.
* Subtype to all other Streams.
* Together with Cons, an example of an Algebraic Data Type (ADT).

### Case Class [Cons](Stream.scala#L312)
* Represents a contravarient stream of type A.
* Together with Empty, an example of an Algebraic Data Type (ADT).

### Object [Stream](Stream.scala#L314-L420)
* Companion object to the Stream trait.
* Contains "smart" constructors.
* Contains a variadic strict stream constuctor factory method (apply).

### Program [StreamTest](exerciseCode/StreamTest.scala)
* A program that exercises fpinscala.laziness.Stream.

### Program [infiniteStreamTest](exerciseCode/infiniteStreamTest.scala)
* Using infinite streams with fpinscala.laziness.Stream.

### Program [scalaInfiniteStreamTest](exerciseCode/scalaInfiniteStreamTest.scala)
* Using infinite streams with standard scala libraries.

### Program [scanRightTest](exerciseCode/scanRightTest.scala)
* Test scanRight with fpinscala.laziness.Stream.

### Program [hasSubsequenceStreamTest](exerciseCode/hasSubsequenceStreamTest.scala)
* Test hasSubsequenceStreamTest with fpinscala.laziness.Stream.

### Program [foldLeftTest](exerciseCode/foldLeftTest.scala)
* Compare foldLeft vs foldRight with fpinscala.laziness.Stream.

Exercises teaching the distinction between laziness and
non-strictness.  A function being lazy means it only evaluates
its arguments if there is a need.  This is sometimes refered to
by "call by need" or "called by name."  Most languages are strict,
they fully evaluate their arguments before they execute.  In C/C++
and Python, short circuit && and || are "functions" which are lazy
in their 2nd arguments.  A function f is strict if the expression
f(x) evaluates to bottom for all x that evaluate to bottom.

Scala is strict by default; it requires special syntax to to
"call by name."  Non-strictness allows right folds to manipulte
infinite data structures in almost metaphysical ways.  What stops
the recursion is this laziness/non-strictness.  It allows for very
efficient Stream compositions.

I have also added a withFilter method to enable guards in "for"
comprehensions in an efficient manner.  I modeled this after what
was done scala.Option.
