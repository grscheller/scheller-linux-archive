# Functional Programming in Scala Book Exercises

## Handling errors without exceptions.

Chapter 4.

Implementing Option and Either error handling data structures
while working through the exercises in  "Functional Programming
in Scala" by Paul Chiusana and Runar Bjarnason.

### Package fpinscala.errorhandling.Option
   * A datatype containing a successful value or 
     indicating a failure occured.

### Package fpinscala.errorhandling.Either
   * A datatype containing either one or another
     of two possible types of values.

Book exercises give practice with combinators.  Combinators
are higher order functions used to manipulate ADT's.  They
allow you to avoid having to pattern match on the internal
structure of the ADT's.  Also, the important concept of
referential transparency is further explored.  Java exceptions
are shown to not be referential transparent.

Methods are being defined in the traits as opposed to
the companion objects.
