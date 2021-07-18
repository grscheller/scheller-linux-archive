package fpinscala.chap02.gettingstarted

// Comment - start off with something familiar to OO programmer
/*
   Longer Multi-line comment

   Using the MyModule "singular" object as
   a mamespace.

*/
/** Documentation comment for MyModule */
object myModule {

  // Note that "if ... else ..." is a an expression
  //   and returns a value, Int in this case, but
  //   could be (): Unit as main does.
  // Body of this function is a single expression.
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  // The return type is String and is being
  //   inferred by the scala compiler.  The
  //   return types on abs and main methods
  //   are redundant.
  // Calls the format method of String class.
  // Functions/methods return the last expression
  //  in the {} expresion block.
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  // This method is not "pure" because it
  //   has a side effect - it changes the state of
  //   my videomonitor.
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

}

/* Refactored version of MyModule */
object myModuleRefactored {

  import myModule._

  // Standard Lisp accumulator trick.
  //   Uses tail recursion optimization
  //   even without the annotation.
  def factorial(n: Int) = {
    @annotation.tailrec  // Compile fails if not tail recursive.
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

  def main(args: Array[String]) = {
    println(formatResult("absolute value of", -42, abs))
    println(formatResult("factorial of", 11, factorial))
    println(formatResult("fibonacci element", 42, fibonacci))
  }
}
