package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Par object
 *
 *  Started after exercise 7.4
 */
object ParTest2 {

  // Model an expensive calculations with
  // a less than stellar implementations
  // of the factorial functon.
  def fib(n: Long): Long =
    if (n < 2) n else fib(n-1) + fib(n-2)

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(5)

    var fibParam = 46L

    // Test Par.asyncF method.
    val parFibF = asyncF(fib)

    val fibMinus3 = parFibF(fibParam - 3)
    val fibMinus2 = map2(fibMinus3, parFibF(fibParam - 4))(_ + _)
    val fibMinus1 = map2(fibMinus2, fibMinus3)(_ + _)
    val fibMinus0 = map2(fibMinus2, fibMinus1)(_ + _)

    print("The " + fibParam + "th Fibonacci number ")
    println {
      val t0 = System.nanoTime
      val hold = run(es)(fibMinus0).get
      val t1 = System.nanoTime
      "is " + hold + " in " + (t1 - t0)/1000000000.0 + " seconds."
    }

    es.shutdown

    println()

  }
}
