package fpinscala.chap02.gettingstarted

/*
   Refactored version of myModules made polymorphic.
*/
object MyPolymorphicModule {

  // These next 4 methods are monomorphic, they can only
  // be used with arguments of specific types.

  def abs(n: Int) =
    if (n < 0) -n
    else n

  def factorial(n: Int) = {
    def loop(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else loop(n-1, n*acc)

    loop(n, 1)
  }

  def fibonacci(n: Int) = {
    def fib(n: Int, nn: Int, mm: Int): Int =
      if (n <= 0)
        nn
      else
        fib(n-1, mm, nn+mm)

    fib(n, 0, 1)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s %d is %d"
    msg.format(name, n, f(n))
  }

  // These next two functions are polymorphic

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n-1), as(n))) loop(n+1)
      else false

    if (as.length < 2) true
    else loop(1)
  }

  def main(args: Array[String]) = {
    println(formatResult("absolute value of", -42, abs))
    println(formatResult("factorial of", 11, factorial))
    println(formatResult("fibonacci element", 42, fibonacci))

    // lambda expressions!
    //   not quite as polymorphic as they could be?
    val isInt5 = (x: Int) => x == 5
    val isDouble5 = (x: Double) => x == 5.0
    val ltIntSqr = (x: Int, y: Int) => x*x < y*y
    val ltDoubleSqr = (x: Double, y: Double) => x*x < y*y

    val foo = Array(3, -4, 5, -6, 10)
    val bar = Array(4.0, 5.9, -2.1, 11.9)

    println("foo has 5 at %d".format(findFirst(foo, isInt5)))
    println("bar has 5 at %d".format(findFirst(bar, isDouble5)))
    println("foo is sqr sorted: " ++ isSorted(foo, ltIntSqr).toString)
    println("bar is sqr sorted: " ++ isSorted(bar, ltDoubleSqr).toString)

    if (isSorted(foo, (x: Int, y: Int) => x*x < y*y))
      println("foo is sqr sorted.")
    else
      println("foo is not sqr sorted.")

    if (isSorted(bar, (x: Double, y: Double) => x*x < y*y))
      println("bar is sqr sorted.")
    else
      println("bar is not sqr sorted.")

  }
}
