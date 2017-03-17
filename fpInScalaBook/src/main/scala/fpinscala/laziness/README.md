#Functional Programming in Scala Book Exercises

##Strictness and laziness

Chapter 5.

Implementing a Stream (Lazy List) data structures while working
through the exercises in  "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason.

###fpinscala.laziness.Stream
   * Implement a lazy list, or stream, using a trait and
     case classes.

Exercises teaching the distinction between laziness and 
non-strictness.  A function being lazy means it only evaluates
its arguments if there is a need.  This is sometimes
refered to by "call by need" or "called by name."  Most
languages are strict, they fully evaluate their arguments
before they execute.  In C/C++ and Python, short circuit
&& and || are "functions" which are lazy in their 2nd
arguments.  A function f is strict if the expression f(x)
evaluates to bottom for all x that evaluate to bottom.

Scala is strict by default; it requires special syntax to
to "call by name."  Non-strictness allows right folds to
manipulte infinite data structures in almost metaphysical
ways.  Laziness/non-strictness is what stops the recursion.
