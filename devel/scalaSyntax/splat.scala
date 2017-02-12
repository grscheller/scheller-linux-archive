package scalaSyntax

object splat {

  // For vatiadic functions, a splat type construct exists.

  def bar(xs: Double*) = xs.foldLeft(0: Double) {_ + _}

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
   *    example: val p = foo _ tupled (2, 3.0, 2.5)
   *     
   */
  def fooTupled: Foo3T => Double = (foo _).tupled

  def main(args: Array[String]) = {

    val xs: Array[Double] = Array(1,2,3,4,5,6,7,8,9,10)
    val ys: List[Double]  =  List(1,2,3,4,5,6,7,8,9,10)

    print("xs = "); println(xs.toString)  // Java objects are brain dead.
                                          // Who but the compiler cares
                                          // what the bloody reference is?
    println("xs = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)")
    print("ys = "); println(ys)

    println("Test variadic function:")

    print("\nbar(1,2,3,4,5,6,7,8,9,10) = ")
    println(bar(1,2,3,4,5,6,7,8,9,10))
    print("bar(xs: _*) = "); println(bar(xs: _*))
    print("bar(ys: _*) = "); println(bar(ys: _*))

    println("\nTest fixed-arity function:")

    print("foo(2, 3.0, 2.5) = ")
    println(foo(2, 3.0, 2.5))

    print("fooTupled((2, 3.0, 2.5)) = ")
    println(fooTupled((2, 3.0, 2.5)))

    print("foo _ tupled (2, 3.0, 2.5) = ")
    println(foo _ tupled (2, 3.0, 2.5))

    print("(foo(_,_,_)).tupled((2, 3.0, 2.5)) = ")
    println((foo(_,_,_)).tupled((2, 3.0, 2.5)))

  }

}
