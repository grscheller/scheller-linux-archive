# Property-based testing

Chapter 8.

Exploring how to decouple the specification of program behavior from
the creation of test cases while working through the exercises in
"Functional Programming in Scala" by Paul Chiusana and Runar Bjarnason.

## Package [fpinscala.testing](fpinScalaCheck.scala)

* package used for property based testing

## Case Class Prop [fpinscala.testing.Prop](fpinScalaCheck.scala#L16-L60)

* represents some property we wish to test

## Companion Object [fpinscala.testing.Prop](fpinScalaCheck.scala#L62-L178)

* used as a namespace for Prop related type aliases

## Case Class [fpinscala.testing.Gen](fpinScalaCheck.scala#L180-L230)

* represents a generator of test cases
* wraps a fpinscala Rand[A] which wraps a State[RNG,A]

## Companion Object [fpinscala.testing.Gen](fpinScalaCheck.scala#L232-L275)

* utility functions for Gen case class
* contains implicit defs for Gen case class
* contains object `**` for matching tuples within Gen pattern matches

## Case Class [fpinscala.testing.SGen](fpinScalaCheck.scala#L277-L296)

* generate Gens of given sizes
* a Gen can be converted to an SGen via an implicit def or its unsized method

## Program [genTest](exerciseCode/genTest.scala)

* used as feedback while developing the Gen type
* actual use of the library will be done under the test/ source tree

## Program [sgenTest](exerciseCode/sgenTest.scala)

* used as feedback while developing the SGen and Prop data types

## Program [checkTest](exerciseCode/checkTest.scala)

* used as feedback while developing the Prop.check method
* used while adding Proved case class
