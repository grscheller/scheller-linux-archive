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
  // non tail recursive Fibonacci function.
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
    print("\nCompare Par.unit vs. Par.lazyUnit:")
    val fibParameter1 = 43L

    println("\nCreate Par via unit.")
    val fibu = Par.unit(fibPoor(fibParameter1))
    println("Par created.")

    println("\nCreate Par via lazyUnit.")
    val fiblu = Par.lazyUnit(fibPoor(fibParameter1))
    println("Par created.")

    println("\nCreate Future from unit.")
    val fibuFuture = Par.run(es)(fibu)
    println("Future created.")

    println("\nGet value of Future from unit value.")
    val fibuValue = fibuFuture.get
    println("fibPoor(" + fibParameter1 + ") = " + fibuValue)

    println("\nRun Future from lazyUnit.")
    val fibluFuture = Par.run(es)(fiblu)
    println("Future created.")

    println("\nGet value of Future from lazyUnit value.")
    val fibluValue = fibluFuture.get
    println("fibPoor(" + fibParameter1 + ") = " + fibluValue)

    // Calculate in parallel
    println("\nRecalculate with some parallelism.")
    val fibMinus4 = Par.lazyUnit(fibPoor(fibParameter1 - 4))
    val fibMinus3 = Par.lazyUnit(fibPoor(fibParameter1 - 3))
    val fibMinus2 = Par.map2(fibMinus4, fibMinus3)(_ + _)
    val fibMinus1 = Par.map2(fibMinus3, fibMinus2)(_ + _)
    val fibMinus0 = Par.map2(fibMinus2, fibMinus1)(_ + _)
    print("The " + fibParameter1 + " Fibonacci number: ")
    println(Par.run(es)(fibMinus0).get)

    // Test order of execution.
    println("\nTest order of execution:")

    val par1: Par[Int] = Par.lazyUnit {
      print("<step1>")
      100
    }
    val par2: Par[Int] = Par.unit {
      print("<step2>")
      140
    }
    val par3: Par[Int] = Par.lazyUnit {
      print("<step3>")
      2
    }
    val par4 = Par.map2(par2, par1)((x,y) => {
      print("<step4>")
      x - y
    })
    val par5 = Par.map2(par4, par3)((x,y) => {
      print("<step5>")
      x + y
    })
    val par6 = Par.map2(par4, par2)((x,y) => {
      print("<step6>")
      x * y
    })
    print("<run>")
    println(Par.run(es)(par5).get)
    println(Par.run(es)(par5).get)
    println(Par.run(es)(par4).get)
    println(Par.run(es)(par6).get)
    val fut5 = Par.run(es)(par5)
    println(fut5.get)
    println(fut5.get)
    val fut1 = Par.run(es)(par1)
    println(fut1.get)
    println(fut1.get)
    val fut2 = Par.run(es)(par2)
    println(fut2.get)
    println(fut2.get)

    // Test Map2Future isDone, isCancelled, and cancel methods.
    val fibParameter2 = 45L
    println("\nTest isDone method of future given by the es:")
    val longRunner1 = Par.lazyUnit(fibPoor(fibParameter2))
    val longRunner1Fut = Par.run(es)(longRunner1)

    while (!longRunner1Fut.isDone) {
      println("Task not done.")
      Thread.sleep(1000)
    }
    print("The " + fibParameter2 + " Fibonacci number: ")
    println(longRunner1Fut.get)

    println("\nTest isDone Future method for Map2Future:")
    val notUsed = Par.unit(42L)
    val longRunner2 = Par.map2(longRunner1, notUsed)((x, y) => x)
    val longRunner2Fut = Par.run(es)(longRunner1)

    /* Need to fix Map2Future !!!
     *   It does not start calculation until the 
     *   get method is called.  It should start 
     *   the calculation when run method called.
     */
//      while (!longRunner2Fut.isDone) {
//        println("Task not done.")
//        Thread.sleep(1000)
//    }
    print("The " + fibParameter2 + " Fibonacci number: ")
    println(longRunner2Fut.get)

    es.shutdown

    println()

  }

}
