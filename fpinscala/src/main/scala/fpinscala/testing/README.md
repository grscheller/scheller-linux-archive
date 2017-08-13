## Property-based testing

Chapter 8.

Exploring how to decouple the specification of program behavior from
the creation of test cases while working through the exercises in
"Functional Programming in Scala" by Paul Chiusana and Runar Bjarnason.

### Package [fpinscala.testing](fpinScalaCheck.scala)
* Package used for property based testing.

### Case class Prop [fpinscala.testing.Prop](fpinScalaCheck.scala#L14-L38)
* Represents some property we wish to test.
* Won't always update line numbers until I am done with chapter.

### Companion object [fpinscala.testing.Prop](fpinScalaCheck.scala#L40-L79)
* Used as a namespace for Prop related type aliases.
* Won't always update line numbers until I am done with chapter.

### Case class [fpinscala.testing.Gen](fpinScalaCheck.scala#L81-L101)
* Generator of test cases.
* Wraps a fpinscala Rand[A] which is a State[RNG,A] type alias.
* Won't always update line numbers until I am done with chapter.

### Companion object [fpinscala.testing.Gen](fpinScalaCheck.scala#L103-L123)
* Won't always update line numbers until I am done with chapter.

### Program [prelimGenTest](exerciseCode/fpinScalaCheckTest.scala)
* Program to exercise package fpinscala.testing data types..
* Actual use of the library will be done under the test/ source tree.

