package grokScala.implicits 
// Example showing syntax how to use implicit conversions

import scala.language.implicitConversions  // compiler warning told me to import this

object MyImplicitConversions {
  implicit def doubleToInt(x: Double): Int = x.toInt
  implicit def IntToIntWrapper(jj: Int): IntWrapper = IntWrapper(jj)
  implicit def DoubleToIntWrapper(jj: Double): IntWrapper = IntWrapper(doubleToInt(jj))

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

class PreferedName(name: String) {
  def getName = name
}

object Main {

  import MyImplicitConversions._  // could be used to determine which
                                  // implicit conversions to make visible

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
    print("4.tripleMe = "); println(4.tripleMe)  // add a method to Int
    print("foo.tripleMe = "); println(foo.doubleMe)  // add a method to Double
    print("bar x 10 = "); println(bar x 10)

    val geoffrey: PreferedName = new PreferedName("Geoffrey")
    // sayHello   // Doesn't find geoffrey???
    sayHello(new PreferedName("Beowulf"))
    sayHello(geoffrey)
  }

}
