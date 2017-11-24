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

### Companion Object [fpinscala.testing.Prop](fpinScalaCheck.scala#L61-L173)
* Used as a namespace for Prop related type aliases.
* Won't always update line numbers until I am done with chapter.

### Case Class [fpinscala.testing.Gen](fpinScalaCheck.scala#L175-L225)
* Represents a generator of test cases.
* Wraps a fpinscala Rand[A] which wraps a State[RNG,A].

### Companion Object [fpinscala.testing.Gen](fpinScalaCheck.scala#L227-L270)
* Utility functions for Gen case class.
* Contains implicit defs for Gen case class.
* Contains object `**` for matching tuples within Gen pattern matches.

### Case Class [fpinscala.testing.SGen](fpinScalaCheck.scala#L272-L292)
* Generate Gens of given sizes.
* A Gen can be converted to an SGen via an implicit def or its unsized method.

### Program [genTest](exerciseCode/genTest.scala)
* Used as feedback while developing the Gen type.
* Actual use of the library will be done under the test/ source tree.

### Program [sgenTest](exerciseCode/sgenTest.scala)
* Used as feedback while developing the SGen and Prop data types.

### Program [checkTest](exerciseCode/checkTest.scala)
* Used as feedback while developing the Prop.check method.
* Used while adding Proved case class.

