package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.javaFutures.Par
import Par._

/** Test fpinscala.parallelism.Blocking object
 *
 *  This is an imperitive application using
 *  java.concurent based constructs.  It also
 *  uses fpinscala.parallelism.Par to generate
 *  java.util.concurrent.Future's in a purely
 *  functional way.
 *
 *  Started after initial implementation of
 *  fpinscala.parallelism package.
 *
 */
object ParJavaFutures1 {

  // Model an expensive calculations with
  // a less than stellar implementations
  // of the factorial functon.
  def fib(n: Long): Long =
    if (n < 2) n else fib(n-1) + fib(n-2)

  def main(args: Array[String]): Unit = {

    // Done before exercise 7.3

    val a100 = unit[Int](100)
    val b500 = unit(500: Int)
    val  c50 = unit(50)

    val ab = a100.map2(b500)(_ + _)
    val abc = fork(ab.map2(c50)(_ - _))

    val es = Executors.newFixedThreadPool(4)
    val abcFuture = abc.future(es)

    println("\n(100 + 500) - 50 = " + abcFuture.get)

    // Done after exercise 7.3

    val fibParameter1 = 43L

    // Compare Par.unit vs. Par.lazyUnit
    print("\nCompare Par.unit vs. Par.lazyUnit:")

    println("\nCreate Par via unit.")
    val fibu = unit(fib(fibParameter1))
    println("Par created.")

    println("\nCreate Par via lazyUnit.")
    val fiblu = lazyUnit(fib(fibParameter1))
    println("Par created.")

    println("\nCreate Future from unit.")
    val fibuFuture = fibu.future(es)
    println("Future created.")

    println("\nGet value of Future from unit value.")
    val fibuValue = fibuFuture.get
    println("fib(" + fibParameter1 + ") = " + fibuValue)

    println("\nRun Future from lazyUnit.")
    val fibluFuture = fiblu.future(es)
    println("Future created.")

    println("\nGet value of Future from lazyUnit value.")
    val fibluValue = fibluFuture.get
    println("fib(" + fibParameter1 + ") = " + fibluValue)

    // Calculate in parallel
    println("\nRecalculate with some parallelism.")

    val fibMinus4 = lazyUnit(fib(fibParameter1 - 4))
    val fibMinus3 = lazyUnit(fib(fibParameter1 - 3))
    val fibMinus2 = fibMinus4.map2(fibMinus3)(_ + _)
    val fibMinus1 = fibMinus3.map2(fibMinus2)(_ + _)
    val fibMinus0 = fibMinus2.map2(fibMinus1)(_ + _)
    print("The " + fibParameter1 + " Fibonacci number: ")
    println(fibMinus0.future(es).get)

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
    print("<map2>")
    val par4 = par2.map2(par1) {
      (x,y) => {
        print("<step4>")
        x - y
      }
    }
    print("<map2>")
    val par5 = par4.map2(par3) {
      (x,y) => {
        print("<step5>")
        x + y
      }
    }
    print("<map2>")
    val par6 = par4.map2(par2) {
      (x,y) => {
        print("<step6>")
        x * y
      }
    }
    print("<run>")
    println(par5.run(es))
    print("<run>")
    println(par5.run(es))
    print("<run>")
    println(par4.run(es))
    print("<run>")
    println(par6.run(es))
    print("<future>")
    val fut5 = par5.future(es)
    print("<get>")
    println(fut5.get)
    print("<get>")
    println(fut5.get)
    print("<future>")
    val fut1 = par1.future(es)
    print("<get>")
    println(fut1.get)
    print("<get>")
    println(fut1.get)
    print("<future>")
    val fut2 = par2.future(es)
    print("<get>")
    println(fut2.get)
    print("<get>")
    println(fut2.get)

    val fibParameter2 = 45L

    // Test isDone method for future provided by the es.
    println("\nTest isDone method of future given to us by the es:")

    val longRunner1 = lazyUnit(fib(fibParameter2))
    val longRunner1_Fut = longRunner1.future(es)

    while (!longRunner1_Fut.isDone) {
      println("Task not done.")
      Thread.sleep(1000)
    }
    print("The " + fibParameter2 + " Fibonacci number: ")
    println(longRunner1_Fut.get)

    // Test isDone method for future provided by map2.
    println("\nTest isDone Future method for Map2Future:")

    val ultimateAns = unit(42L)
    val longRunner2 = longRunner1.map2(ultimateAns) {(x, y) => x - y}
    val longRunner2_Fut = longRunner2.frozenFuture(es)

    while (!longRunner2_Fut.isDone) {
      println("Task not done.")
      Thread.sleep(1000)
    }
    print("The " + fibParameter2 + " Fibonacci number - ultamateAns: ")
    println(longRunner2_Fut.get)
    // The following should return immediately.
    print("The " + fibParameter2 + " Fibonacci number - ultimateAns: ")
    println(longRunner2_Fut.get)

    // Syntax and stictness check
    println("\nThe map2 method seems to be strict.")
    val longRunner3 = ultimateAns.map2(longRunner1) {(x, _) => x}
    val longRunner3_Fut = longRunner3.future(es)

    print("The ultimate answer to life, the universe, and everything is ")
    print(longRunner3_Fut.get)
    println(".")

    // Test isCancelled, and cancel methods for completed futures.
    println("\nTry cancelling futures which are done:")

    val futureDoneES = longRunner1_Fut
    val futureDoneMap2 = longRunner2_Fut

    println("futureDoneES.isDone: "        + futureDoneES.isDone       )
    println("futureDoneES.isCancelled: "   + futureDoneES.isCancelled  )
    println("futureDoneES.cancel(false): " + futureDoneES.cancel(false))
    println("futureDoneES.isCancelled: "   + futureDoneES.isCancelled  )
    println("futureDoneES.cancel(true): "  + futureDoneES.cancel(true) )
    println("futureDoneES.isCancelled: "   + futureDoneES.isCancelled  )
    println()
    println("futureDoneMap2.isDone: "        + futureDoneMap2.isDone       )
    println("futureDoneMap2.isCancelled: "   + futureDoneMap2.isCancelled  )
    println("futureDoneMap2.cancel(false): " + futureDoneMap2.cancel(false))
    println("futureDoneMap2.isCancelled: "   + futureDoneMap2.isCancelled  )
    println("futureDoneMap2.cancel(true): "  + futureDoneMap2.cancel(true) )
    println("futureDoneMap2.isCancelled: "   + futureDoneMap2.isCancelled  )

    val fibParameter3 = 46L

    // Test isCancelled, and cancel methods for a running future.
    println("\nTry cancelling a future which is running:")

    val longRunner4 = lazyUnit(fib(fibParameter3))
    val longRunner4_Fut = longRunner4.frozenFuture(es)

    println("longRunner4_Fut.isDone: "        + longRunner4_Fut.isDone       )
    println("longRunner4_Fut.isCancelled: "   + longRunner4_Fut.isCancelled  )
    println("longRunner4_Fut.cancel(false): " + longRunner4_Fut.cancel(false))
    println("longRunner4_Fut.isCancelled: "   + longRunner4_Fut.isCancelled  )
    println("longRunner4_Fut.isDone: "        + longRunner4_Fut.isDone       )
    println("longRunner4_Fut.cancel(true): "  + longRunner4_Fut.cancel(true) )
    println("longRunner4_Fut.isCancelled: "   + longRunner4_Fut.isCancelled  )
    println("longRunner4_Fut.isDone: "        + longRunner4_Fut.isDone       )

    // Test isCancelled, and cancel methods for a running map2 future.
    println("\nTry cancelling map2 future which just started running:")

    val longRunner5 = lazyUnit(fib(fibParameter3)).map2(
                        lazyUnit(fib(fibParameter2)))(_ - _)
    val longRunner5_Fut = longRunner5.future(es)

    try {
        print("longRunner5_Fut.isDone: ")
        println(longRunner5_Fut.isDone  )
        print("longRunner5_Fut.isCancelled: ")
        println(longRunner5_Fut.isCancelled  )
        print("longRunner5_Fut.cancel(false): ")
        println(longRunner5_Fut.cancel(false)  )
        print("longRunner5_Fut.isCancelled: ")
        println(longRunner5_Fut.isCancelled  )
        print("longRunner5_Fut.isDone: ")
        println(longRunner5_Fut.isDone  )
        print("longRunner5_Fut.cancel(true): ")
        println(longRunner5_Fut.cancel(true)  )
        print("longRunner5_Fut.isCancelled: ")
        println(longRunner5_Fut.isCancelled  )
        print("longRunner5_Fut.isDone: ")
        println(longRunner5_Fut.isDone  )
    } catch {
        case ex: TimeoutException      => println("Timed out!")
        case ex: CancellationException => println("Somebody cancelled me!")
        case ex: InterruptedException  => println("Interupted!")
        case ex: ExecutionException    => println("ExecutionException!")
    }

    // Test isCancelled, and cancel methods for a longer running map2 future.
    println("\nAfter a while, try cancelling map2 future:")

    val longRunner6 = lazyUnit(fib(fibParameter3)).map2(
                        lazyUnit(fib(fibParameter2)))(_ - _)
    val longRunner6_Fut = longRunner6.future(es)

    try {
        print("longRunner6_Fut.isDone: ")
        println(longRunner6_Fut.isDone  )
        println("Sleep 5 seconds"); Thread.sleep(5000)
        print("longRunner6_Fut.isDone: ")
        println(longRunner6_Fut.isDone  )
        println("Sleep 5 seconds"); Thread.sleep(5000)
        print("longRunner6_Fut.cancel(false): ")
        println(longRunner6_Fut.cancel(false)  )
        print("longRunner6_Fut.isCancelled: ")
        println(longRunner6_Fut.isCancelled  )
        print("longRunner6_Fut.isDone: ")
        println(longRunner6_Fut.isDone  )
        print("longRunner6_Fut.cancel(true): ")
        println(longRunner6_Fut.cancel(true)  )
        print("longRunner6_Fut.isCancelled: ")
        println(longRunner6_Fut.isCancelled  )
        print("longRunner6_Fut.isDone: ")
        println(longRunner6_Fut.isDone  )
    } catch {
        case ex: TimeoutException      => println("Timed out!")
        case ex: CancellationException => println("Somebody cancelled me!")
        case ex: InterruptedException  => println("Interupted!")
        case ex: ExecutionException    => println("ExecutionException!")
    }

    // Test isCancelled, and cancel methods for a not yet running map2 future.
    println("\nTry cancelling map2 future which is not yet running:")

    val longRunner7 = lazyUnit(fib(fibParameter3)).map2(
                        lazyUnit(fib(fibParameter2)))(_ - _)
    val longRunner7_Fut = longRunner7.frozenFuture(es)

    try {
        print("longRunner7_Fut.isCancelled: ")
        println(longRunner7_Fut.isCancelled  )
        print("longRunner7_Fut.cancel(false): ")
        println(longRunner7_Fut.cancel(false)  )
        print("longRunner7_Fut.isCancelled: ")
        println(longRunner7_Fut.isCancelled  )
        print("longRunner7_Fut.isDone: ")
        println(longRunner7_Fut.isDone  )
        print("longRunner7_Fut.get() = ")
        println(longRunner7_Fut.get     )
    } catch {
        case ex: TimeoutException      => println("Timed out!")
        case ex: CancellationException => println("Somebody cancelled me!")
        case ex: InterruptedException  => println("Interupted!")
        case ex: ExecutionException    => println("ExecutionException!")
    }

    // Test get with timeout after a get.
    println("\nTry get with timeout after a get:")

    val longRunner8 = lazyUnit(fib(fibParameter3)).map2(
                        lazyUnit(fib(fibParameter2)))(_ - _)
    val longRunner8_Fut = longRunner8.frozenFuture(es)

    print("longRunner8_Fut.get() = ")
    println(longRunner8_Fut.get()  )
    print("longRunner8_Fut.get(4, TimeUnit.SECONDS) = ")
    println(longRunner8_Fut.get(4, TimeUnit.SECONDS)  )

    // Test get with too short timeout, followed by get.
    println("\nTry get with too short timeout, followed by get:")

    val longRunner9 = lazyUnit(fib(fibParameter3)).map2(
                        lazyUnit(fib(fibParameter2)))(_ - _)
    val longRunner9_Fut = longRunner9.frozenFuture(es)

    try {
        print("longRunner9_Fut.get(4, TimeUnit.SECONDS) = ")
        println(longRunner9_Fut.get(4, TimeUnit.SECONDS))
    } catch {
        case ex: TimeoutException      => println("Timed out!")
        case ex: CancellationException => println("Somebody cancelled me!")
        case ex: InterruptedException  => println("Interupted!")
        case ex: ExecutionException    => println("ExecutionException!")
    }
    print("longRunner9_Fut.get = ")
    println(longRunner9_Fut.get)

    // lazyUnits can cause some data sharing between futures from same Par.
    println("\nlazyUnits can cause some data sharing between futures from")
    println("the same Par, the second future's get method returns immediately:")

    val longRunner10 = lazyUnit(fib(fibParameter3)).map2(
                         lazyUnit(fib(fibParameter2)))(_ - _)
    val longRunner10_Fut1 = longRunner9.frozenFuture(es)
    val longRunner10_Fut2 = longRunner9.frozenFuture(es)

    print("longRunner10_Fut1.get = "); println(longRunner10_Fut1.get)
    print("longRunner10_Fut2.get = "); println(longRunner10_Fut2.get)

    es.shutdown

    println()

  }

}
