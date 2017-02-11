package scalaExamples

object splat {

  // For fixed-arity functions, concrete types

  def foo(n: Int, x: Double, y: Double): Double = n*x + y

  type Foo3T = (Int, Double, Double)

  /** Example of tupling.
   *
   *  Turning a 3 argument function into a
   *  single argument function of a 3-tuple.
   *
   *  Unsugared:     (foo(_,_,_)).tupled
   *
   *  Full sacarine: import scala.language.postfixOps
   *                 foo _ tupled
   *
   *    when fully applied, import not needed
   *    example: val p = foo _ tupled (2, 3.0)
   *     
   */
  def fooTupled: Foo3T => Double = (foo _).tupled

  def main(args: Array[String]) = {

    println(foo(2, 3.0, 2.5))
    println(fooTupled((2, 3.0, 2.5)))

  }

}
