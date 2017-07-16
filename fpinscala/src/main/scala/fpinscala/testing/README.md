## Property-based testing

Chapter 7.

Exploring how to decouple the specification of program behavior from<br>
the creation of test cases while working through the exercises in<br>
"Functional Programming in Scala" by Paul Chiusana and Runar Bjarnason.

### Package [fpinscala.testing](Gen.scala)
* Package used for property based testing.

### Trait Prop [fpinscala.testing.Prop](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/testing/Gen.scala)
* Trait representing some property we wish to test.
* Won't put line numbers in until I am done with chapter.

### Companion object [fpinscala.testing.Prop](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/testing/Gen.scala)
* Used as a namespace for Prop related type aliases.
* Won't put line numbers in until I am done with chapter.

### Case class [fpinscala.testing.Gen](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/testing/Gen.scala)
* Generator of test cases.
* Wraps a fpinscala Rand[A] which is a State[RNG,A] type alias.
* Won't put line numbers in until I am done with chapter.

### Companion object [fpinscala.testing.Gen](https://github.com/grscheller/scheller-linux-archive/blob/master/fpinscala/src/main/scala/fpinscala/testing/Gen.scala)
* Won't put line numbers in until I am done with chapter.

### Program [prelimGenTest](exerciseCode/prelimGenTest.scala)
* Program to exercise package fpinscala.testing data types..
* Actual use of the library will be done under test/ tree.

