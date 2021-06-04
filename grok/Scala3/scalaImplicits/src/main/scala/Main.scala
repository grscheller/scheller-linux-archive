package scalaImplicits
// Package showing syntax how to use implicit conversions in Scala 3.
//
//   See grok/Scala2/learnScala/implicits for a Scala 2
//   version using the overloaded explicit key word.  For
//   backward compatibility, that version would still compile
//   in Scala 3, but would be deprecated.
//
//   The implicit class Times still needs to be ported over
//   to Scala 3 syntax.

import scala.language.implicitConversions

object MyGivens {
  given doubleToInt: Conversion[Double, Int] = _.toInt
  given intToIntWrapper: Conversion[Int, IntWrapper] = IntWrapper(_)
  given doubleToIntWrapper: Conversion[Double, IntWrapper] = (x: Double) =>IntWrapper(x.toInt)

  // implicit classes must have a single argument constructor,
  // cannot be a case class, and must be located within another
  // object, class, or  trait
  implicit class Times(first: Int) {
    def x(second: Int) = IntWrapper(first * second)
  }
}

case class IntWrapper(ii: Int) {
  def doubleMe = ii*2
  def tripleMe = ii*3
}

object Main {

  import scalaImplicits.MyGivens.doubleToInt
  import scalaImplicits.MyGivens.intToIntWrapper
  import scalaImplicits.MyGivens.doubleToIntWrapper
  import scalaImplicits.MyGivens.Times

  class PreferedName(name: String) {
    def getName = name
  }

  def sayHello(implicit preferedName: PreferedName) = {
    val nameStr = preferedName.getName
    println(s"Hello, $nameStr")
  }

  def main(args: Array[String]) = {

    val foo: Double = 42.314159
    val bar: Int = foo   // implicit conversion prevents this
                         // from being a type mismatch error
    print("foo = "); println(foo)
    print("bar = "); println(bar)
    print("4.tripleMe = "); println(4.tripleMe)      // add a method to Int
    print("foo.doubleMe = "); println(foo.doubleMe)  // add a method to Double
    print("bar x 10 = "); println(bar x 10)

    given geoffrey: PreferedName = new PreferedName("Geoffrey")
    val beowulf: PreferedName = new PreferedName("Beowulf")
    sayHello
    sayHello(using beowulf)
  }

}
