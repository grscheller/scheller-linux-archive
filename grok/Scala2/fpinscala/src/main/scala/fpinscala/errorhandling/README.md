# Handling errors without exceptions

Chapter 4.

Implementing Option and Either error handling data structures while
working through the exercises in "Functional Programming in Scala"
by Paul Chiusana and Runar Bjarnason.

## Trait [fpinscala.errorhandling.Option](Option.scala#L3-L79)

* datatype indicating a successful value or failure

## Program [OptionTest](exerciseCode/OptionTest.scala)

* program that parses to exercise fpinscala.errorhandling.Option

## Trait [fpinscala.errorhandling.Either](Either.scala#L3-L66)

* datatype containing a value of either one of two possible types

## Program [EitherTest](exerciseCode/EitherTest.scala)

* program that parses to exercise fpinscala.errorhandling.Either

## Program [scalaErrorhandling](exerciseCode/scalaErrorhandling.scala)

* parsing and error handling using the scala libraries
* does not use fpinscala.errorhandling

Book exercises give practice with combinators.  Combinators are
higher order functions used to manipulate ADT's.  They allow you
to avoid having to pattern match on the internal structure of the
ADT's.  Also, the important concept of referential transparency is
further explored.  Java exceptions are shown to not be referential
transparent.

Methods are now being defined in the traits as opposed to
within the companion objects.
