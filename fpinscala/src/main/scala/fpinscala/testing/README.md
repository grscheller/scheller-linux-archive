## Property-based testing

Chapter 8.

Exploring how to decouple the specification of program behavior from
the creation of test cases while working through the exercises in
"Functional Programming in Scala" by Paul Chiusana and Runar Bjarnason.

### Package [fpinscala.testing](fpinScalaCheck.scala)
* Package used for property based testing.

### Case Class Prop [fpinscala.testing.Prop](fpinScalaCheck.scala#L17-L59)
* Represents some property we wish to test.
* Won't always update line numbers until I am done with chapter.

### Companion Object [fpinscala.testing.Prop](fpinScalaCheck.scala#L61-L172)
* Used as a namespace for Prop related type aliases.
* Won't always update line numbers until I am done with chapter.

### Case Class [fpinscala.testing.Gen](fpinScalaCheck.scala#L174-L220)
* Generator of test cases.
* Wraps a fpinscala Rand[A] which wraps a State[RNG,A].
* Won't always update line numbers until I am done with chapter.

### Companion Object [fpinscala.testing.Gen](fpinScalaCheck.scala#L225-L266)
* Won't always update line numbers until I am done with chapter.

### Case Class [fpinscala.testing.SGen](fpinScalaCheck.scala#L268-L283)
* Generates a Gen of a given size.
* A Gen can be converted to an SGen via an implicit def or its unsized method.
* Won't always update line numbers until I am done with chapter.

### Program [genTest](exerciseCode/genTest.scala)
* Used as feedback while developing the Gen type.
* Actual use of the library will be done under the test/ source tree.

### Program [sgenTest](exerciseCode/sgenTest.scala)
* Used as feedback while developing the SGen and Prop data types.

### Program [checkTest](exerciseCode/checkTest.scala)
* Used as feedback while developing the Prop.check method.
* Used while adding Proved case class.

