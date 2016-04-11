/*
   Refactored version of MyModule.
*/
object MyModuleRefactored {

  def abs(n: Int) =
    if (n < 0) -n
    else n

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
