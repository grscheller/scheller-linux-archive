package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Par object
 *
 *  This is an imperitive application using
 *  java.concurent based constructs.  It also
 *  uses fpinscala.parallelism.Par to generate
 *  java.util.concurrent.Future's in a purely
 *  functional way.
 *
 */
object ParTest {

  // Model expensive calculations with a
  // non-recursive Fibonacci function.
  def fibPoor(n: Long): Long =
    if (n < 2) n else fibPoor(n-1) + fibPoor(n-2)

  def main(args: Array[String]): Unit = {

    // Done before exercise 7.3

    val a100 = Par.unit[Int](100)
    val b500 = Par.unit(500: Int)
    val  c50 = Par.unit(50)

    val ab = Par.map2(a100, b500)(_ + _)
    val abc = Par.fork((Par.map2(ab, c50)(_ - _)))

    val es = Executors.newFixedThreadPool(6)
    val abcFuture = Par.run(es)(abc)

    println("\n(100 + 500) - 50 = " + abcFuture.get)

    // Done after exercise 7.3

    // Compare Par.unit vs. Par.lazyUnit
    println("\nCompare Par.unit vs. Par.lazyUnit:\n")
    val fibParameter = 43L

    println("\nCreate Par via unit.")
    val fibu = Par.unit(fibPoor(fibParameter))
    println("Par created.")

    println("\nCreate Par via lazyUnit.")
    val fiblu = Par.lazyUnit(fibPoor(fibParameter))
    println("Par created.")

    println("\nCreate Future from unit.")
    val fibuFuture = Par.run(es)(fibu)
    println("Future created.")

    println("\nGet value of Future from unit value.")
    val fibuValue = fibuFuture.get
    println("fibPoor(" + fibParameter + ") = " + fibuValue)

    println("\nRun Future from lazyUnit.")
    val fibluFuture = Par.run(es)(fiblu)
    println("Future created.")

    println("\nGet value of Future from lazyUnit value.")
    val fibluValue = fibluFuture.get
    println("fibPoor(" + fibParameter + ") = " + fibluValue)

    // Calculate in parallel
    println("\nRecalculate with some parallelism.")
    val fibMinus4 = Par.lazyUnit(fibPoor(fibParameter - 4))
    val fibMinus3 = Par.lazyUnit(fibPoor(fibParameter - 3))
    val fibMinus2 = Par.map2(fibMinus4, fibMinus3)(_ + _)
    val fibMinus1 = Par.map2(fibMinus3, fibMinus2)(_ + _)
    val fibMinus0 = Par.map2(fibMinus2, fibMinus1)(_ + _)
    print("The " + fibParameter + " Fibonacci number: ")
    println(Par.run(es)(fibMinus0).get)

    es.shutdown

    println()

  }

}
