package grokScala.splat

object Splat {

  // For variadic functions, a splat type construct
  // exists for variable length datastructures.
  def bar(xs: Double*):Double = xs.foldLeft(0: Double) {_ + _}

  // For fixed-arity functions, we can
  // accomplish splat functionality via tupled:
  def foo(n: Int, x: Double, y: Double): Double = n*x + y
  def fooC(n: Int)(x: Double)(y: Double): Double = n*x + y
  type Tup3 = (Int, Double, Double)
  def foo_T: Tup3 => Double = (foo _).tupled
  def fooC_UT: Tup3 => Double = 
    Function.uncurried(fooC _).tupled

  def main(args: Array[String]) = {

    val ys: List[Double]  =  List(1,2,3,4,5)
    val ss: LazyList[Double] = LazyList(ys: _*)
    val xs: Array[Double] = Array(1,2,3,4,5)

    print("ys = "); println(ys)
    print("ss = "); println(ss)
    println("xs = Array(1.0, 2.0, 3.0, 4.0, 5.0)")

    // Variadic Functions
    println("\nTest variadic functions:")
    print("bar(1,2,3,4,5) = "); println(bar(1,2,3,4,5))
    print("bar(ys: _*) = "); println(bar(ys: _*))
    print("bar(ss: _*) = "); println(bar(ss: _*))
      // since 2.13.0, gives [warn] when splatting an Array directly
    print("bar(xs.toIndexedSeq: _*) = "); println(bar(xs.toIndexedSeq: _*))

    // Test Fixed-arity Functions
    println("\nTest fixed-arity function:")
    print("foo(2, 3.0, 2.5) = "); println(foo(2, 3.0, 2.5))
    print("foo_T((2, 3.0, 2.5)) = "); println(foo_T((2, 3.0, 2.5)))

    print("foo _ tupled (2, 3.0, 2.5) = "); println(foo _ tupled (2, 3.0, 2.5))

    // Test Curried Fixed-arity Functions
    println("\nTest curried fixed-arity function:")
    print("fooC(2)(3.0)(2.5) = ")
    println(fooC(2)(3.0)(2.5))
    print("fooC_UT((2, 3.0, 2.5)) = ")
    println(fooC_UT((2, 3.0, 2.5)))
    print("Function.uncurried(fooC _) tupled (2, 3.0, 2.5) = ")
    println(Function.uncurried(fooC _) tupled (2, 3.0, 2.5))

  }

}
