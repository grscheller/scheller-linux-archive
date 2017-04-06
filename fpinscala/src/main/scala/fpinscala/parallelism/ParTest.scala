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

    val a100 = unit[Int](100)
    val b500 = unit(500: Int)
    val  c50 = unit(50)

    val ab = map2(a100, b500)(_ + _)
    val abc = fork((map2(ab, c50)(_ - _)))

    val es = Executors.newFixedThreadPool(6)
    val abcFuture = run(es)(abc)

    println("\n(100 + 500) - 50 = " + abcFuture.get)

    // Done after exercise 7.3

    // Compare Par.unit vs. Par.lazyUnit
    print("\nCompare Par.unit vs. Par.lazyUnit:")
    val fibParameter1 = 43L

    println("\nCreate Par via unit.")
    val fibu = unit(fibPoor(fibParameter1))
    println("Par created.")

    println("\nCreate Par via lazyUnit.")
    val fiblu = lazyUnit(fibPoor(fibParameter1))
    println("Par created.")

    println("\nCreate Future from unit.")
    val fibuFuture = run(es)(fibu)
    println("Future created.")

    println("\nGet value of Future from unit value.")
    val fibuValue = fibuFuture.get
    println("fibPoor(" + fibParameter1 + ") = " + fibuValue)

    println("\nRun Future from lazyUnit.")
    val fibluFuture = run(es)(fiblu)
    println("Future created.")

    println("\nGet value of Future from lazyUnit value.")
    val fibluValue = fibluFuture.get
    println("fibPoor(" + fibParameter1 + ") = " + fibluValue)

    // Calculate in parallel
    println("\nRecalculate with some parallelism.")
    val fibMinus4 = lazyUnit(fibPoor(fibParameter1 - 4))
    val fibMinus3 = lazyUnit(fibPoor(fibParameter1 - 3))
    val fibMinus2 = map2(fibMinus4, fibMinus3)(_ + _)
    val fibMinus1 = map2(fibMinus3, fibMinus2)(_ + _)
    val fibMinus0 = map2(fibMinus2, fibMinus1)(_ + _)
    print("The " + fibParameter1 + " Fibonacci number: ")
    println(run(es)(fibMinus0).get)

    // Test order of execution.
    println("\nTest order of execution:")

    val par1: Par[Int] = lazyUnit {
      print("<step1>")
      100
    }
    val par2: Par[Int] = unit {
      print("<step2>")
      140
    }
    val par3: Par[Int] = lazyUnit {
      print("<step3>")
      2
    }
    val par4 = map2(par2, par1)((x,y) => {
      print("<step4>")
      x - y
    })
    val par5 = map2(par4, par3)((x,y) => {
      print("<step5>")
      x + y
    })
    val par6 = map2(par4, par2)((x,y) => {
      print("<step6>")
      x * y
    })
    print("<run>")
    println(run(es)(par5).get)
    println(run(es)(par5).get)
    println(run(es)(par4).get)
    println(run(es)(par6).get)
    val fut5 = run(es)(par5)
    println(fut5.get)
    println(fut5.get)
    val fut1 = run(es)(par1)
    println(fut1.get)
    println(fut1.get)
    val fut2 = run(es)(par2)
    println(fut2.get)
    println(fut2.get)

    // Test isDone method for future provided by the es.
    val fibParameter2 = 45L

    println("\nTest isDone method of future given to us by the es:")
    val longRunner1 = lazyUnit(fibPoor(fibParameter2))
    val longRunner1_Fut = run(es)(longRunner1)

    while (!longRunner1_Fut.isDone) {
      println("Task not done.")
      Thread.sleep(1000)
    }
    print("The " + fibParameter2 + " Fibonacci number: ")
    println(longRunner1_Fut.get)

    println("\nTest isDone Future method for Map2Future:")
    val ultimateAns = unit(42L)
    val longRunner2 = map2(longRunner1, ultimateAns)((x, y) => x)
    val longRunner2_Fut = run(es)(longRunner2)

    while (!longRunner2_Fut.isDone) {
      println("Task not done.")
      Thread.sleep(1000)
    }
    print("The " + fibParameter2 + " Fibonacci number: ")
    println(longRunner2_Fut.get)
    print("The " + fibParameter2 + " Fibonacci number: ")
    println(longRunner2_Fut.get)

    val longRunner3 = map2(ultimateAns, longRunner1)((x, _) => x)
    val longRunner3_Fut = run(es)(longRunner3)

    print("The ultimate answer to life, the universe, and everything is ")
    print(longRunner3_Fut.get)
    println(".")

    // Test isCancelled, and cancel methods.
    println("\nTry cancelling isDone futures:")
    val futureDoneES = longRunner1_Fut
    val futureDoneMap2 = longRunner2_Fut

    println("futureDoneES.isDone: " + futureDoneES.isDone)
    println("futureDoneES.isCancelled: " + futureDoneES.isCancelled)
    println("futureDoneES.cancel(false): " + futureDoneES.cancel(false))
    println("futureDoneES.isCancelled: " + futureDoneES.isCancelled)
    println("futureDoneES.cancel(true): " + futureDoneES.cancel(true))
    println("futureDoneES.isCancelled: " + futureDoneES.isCancelled)
    println()
    println("futureDoneMap2.isDone: " + futureDoneMap2.isDone)
    println("futureDoneMap2.isCancelled: " + futureDoneMap2.isCancelled)
    println("futureDoneMap2.cancel(false): " + futureDoneMap2.cancel(false))
    println("futureDoneMap2.isCancelled: " + futureDoneMap2.isCancelled)
    println("futureDoneMap2.cancel(true): " + futureDoneMap2.cancel(true))
    println("futureDoneMap2.isCancelled: " + futureDoneMap2.isCancelled)

    es.shutdown

    println()

  }

}
