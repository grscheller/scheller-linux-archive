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

    val fibParameter1 = 43L

    // Compare Par.unit vs. Par.lazyUnit
    print("\nCompare Par.unit vs. Par.lazyUnit:")

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

    val fibParameter2 = 45L

    // Test isDone method for future provided by the es.
    println("\nTest isDone method of future given to us by the es:")

    val longRunner1 = lazyUnit(fibPoor(fibParameter2))
    val longRunner1_Fut = run(es)(longRunner1)

    while (!longRunner1_Fut.isDone) {
      println("Task not done.")
      Thread.sleep(1000)
    }
    print("The " + fibParameter2 + " Fibonacci number: ")
    println(longRunner1_Fut.get)

    // Test isDone method for future provided by map2.
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

    // Syntax check
    val longRunner3 = map2(ultimateAns, longRunner1)((x, _) => x)
    val longRunner3_Fut = run(es)(longRunner3)

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

    val longRunner4 = lazyUnit(fibPoor(fibParameter3))
    val longRunner4_Fut = run(es)(longRunner4)

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

    val longRunner5 = map2( lazyUnit(fibPoor(fibParameter3))
                          , lazyUnit(fibPoor(fibParameter2))
                          )(_ - _)
    val longRunner5_Fut = run(es)(longRunner5)

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

    val longRunner6 = map2( lazyUnit(fibPoor(fibParameter3))
                          , lazyUnit(fibPoor(fibParameter2))
                          )(_ - _)
    val longRunner6_Fut = run(es)(longRunner6)

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

    val longRunner7 = map2( lazyUnit(fibPoor(fibParameter3))
                          , lazyUnit(fibPoor(fibParameter2))
                          )(_ - _)
    val longRunner7_Fut = run(es)(longRunner7)

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

    val longRunner8 = map2( lazyUnit(fibPoor(fibParameter3))
                          , lazyUnit(fibPoor(fibParameter2))
                          )(_ - _)
    val longRunner8_Fut = run(es)(longRunner8)

    print("longRunner8_Fut.get() = ")
    println(longRunner8_Fut.get()  )
    print("longRunner8_Fut.get(4, TimeUnit.SECONDS) = ")
    println(longRunner8_Fut.get(4, TimeUnit.SECONDS)  )

    // Test get with too short timeout, followed by get.
    println("\nTry get with too short timeout, followed by get:")

    val longRunner9 = map2( lazyUnit(fibPoor(fibParameter3))
                          , lazyUnit(fibPoor(fibParameter2))
                          )(_ - _)
    val longRunner9_Fut = run(es)(longRunner9)

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

    val longRunner10 = map2( lazyUnit(fibPoor(fibParameter3))
                           , lazyUnit(fibPoor(fibParameter2))
                           )(_ - _)
    val longRunner10_Fut1 = run(es)(longRunner9)
    val longRunner10_Fut2 = run(es)(longRunner9)

    print("longRunner10_Fut1.get = "); println(longRunner10_Fut1.get)
    print("longRunner10_Fut2.get = "); println(longRunner10_Fut2.get)

    es.shutdown

    println()

  }

}
