package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.BlockingPar._

/** Test fpinscala.parallelism.Blocking object
 *
 *  Started after exercise 7.4
 */
object ParTest2 {

  import fpinscala.state.rand.{Rand,RNG,LCG}  

  // Utility function to create a bounded list of non-negative integers.
  def makeRandomList(lt: Int, len: Int) =
    Rand.sequence(List.fill(len)(RNG.nonNegativeLessThan(lt)))

  // Model an expensive calculations with a less than
  // stellar implementations of the factorial functon.
  def fib(n: Long): Long =
    if (n < 2) n else fib(n-1) + fib(n-2)

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(550)

    var fibParam = 46L

    // Test Par.asyncF method.
    val parFibF = asyncF(fib)

    val fibMinus3 = parFibF(fibParam - 3)
    val fibMinus2 = map2(fibMinus3, parFibF(fibParam - 4))(_ + _)
    val fibMinus1 = map2(fibMinus2, fibMinus3)(_ + _)
    val fibMinus0 = map2(fibMinus2, fibMinus1)(_ + _)

    print("\nThe " + fibParam + "th Fibonacci number ")
    println {
      val t0 = System.nanoTime
      val hold = run(es)(fibMinus0).get
      val t1 = System.nanoTime
      "is " + hold + " in " + (t1 - t0)/1000000000.0 + " seconds.\n"
    }

    // Test Par.sequence
    println("\nTest Par.sequence:\n")
    
    val listPars: List[Par[Long]] =
      List.iterate(0, (fibParam + 1).toInt)(_ + 1) map (parFibF(_))

    val parList = sequence(listPars)
    val fibNumbersFuture = run(es)(parList)
    var fibNumbers: List[Long] = Nil
    val timeOut = 5  // Seconds
    try {
        // Comment/uncomment for different senarios.
        if (fibNumbersFuture.isDone)
          println("Future is done.")
        else
          println("Future is not done.")
        Thread.sleep(2000)  // Sleep 2 seconds.
        // Thread.sleep(5000)  // Sleep 5 seconds.
        fibNumbersFuture.cancel(false)
        // fibNumbersFuture.cancel(true)
        fibNumbers = fibNumbersFuture.get(timeOut, TimeUnit.SECONDS)
        // fibNumbersFuture.cancel(false)
        // fibNumbersFuture.cancel(true)
    } catch {
        case ex: TimeoutException =>
          println("Timed out! " + timeOut + " seconds not enough.")
        case ex: CancellationException =>
          println("I've been cancelled!")
    } finally {
        if (fibNumbersFuture.isDone)
          println("Future is done.")
        else
          println("Future is not done.")
        if (fibNumbersFuture.isCancelled)
          println("Future is cancelled.")
        else
          println("Future is not cancelled.")
        if (! fibNumbersFuture.isCancelled)
          fibNumbers = fibNumbersFuture.get()
    }
    for (fibNumber <- fibNumbers) println(fibNumber)

    println()

    es.shutdown

    // Test sequence via parFilter and parMap.

    val numThreads_A = 350
    val numThreads_B = 700
    val es_A = Executors.newFixedThreadPool(numThreads_A)
    val es_B = Executors.newFixedThreadPool(numThreads_B)

    // First test with something trivial to see if each
    // implementation actually works.
    println("\nTrivial test Par.parFilter:\n")

    val shortList = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
    val parNo2or3Mults =
      parFilter(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("Filter out multiples of 2 and 3: ")
    println(run(es_A)(parNo2or3Mults).get)

    // Less trivial Par.parFilter test.
    println("\n\nTest Par.parFilter with short calculations:\n")

    val randList = makeRandomList(100, 200)(LCG(234))

    // Test 1:
    print("Time parFilter with " + numThreads_A + " threads: ")
    val futEvenRandList_1A = run(es_A)(parFilter(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs"
    }

    print("Repeat without parallelism:      ")
    println {
      val t0 = System.nanoTime
      val hold = randList filter (_ % 2 == 0)
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs\n"
    }

    // Test 2:
    print("Time parFilter with " + numThreads_A + " threads: ")
    val futEvenRandList_2A = run(es_A)(parFilter(randList)(_ % 5 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs"
    }

    print("Repeat without parallelism:      ")
    println {
      val t0 = System.nanoTime
      val hold = randList filter (_ % 5 == 0)
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs\n"
    }

    // Test 3:
    print("Time parFilter with " + numThreads_B + " threads: ")
    val futEvenRandList_3B = run(es_B)(parFilter(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs"
    }

    print("Repeat without parallelism:      ")
    println {
      val t0 = System.nanoTime
      val hold = randList filter (_ % 2 == 0)
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs\n"
    }

    // Test 4:
    print("Time parFilter with " + numThreads_B + " threads: ")
    val futEvenRandList_4B = run(es_B)(parFilter(randList)(_ % 3 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs"
    }

    print("Repeat without parallelism:      ")
    println {
      val t0 = System.nanoTime
      val hold = randList filter (_ % 3 == 0)
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs\n"
    }

    es_A.shutdown
    es_B.shutdown

    // Test with parMap and longer calculations.
    println("\nTest Par.parMap with a longer calculation:\n")

    val numThreads_fib = 80
    val es_fib = Executors.newFixedThreadPool(numThreads_fib)

    val fibParms = List.range(0,46) map (_.toLong)

    print("Time parMap with " + numThreads_fib + " threads:  ")
    val futFib = run(es_fib)(parMap(fibParms)(fib))
    println {
      val t0 = System.nanoTime
      val hold = futFib.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " seconds"
    }

    es_fib.shutdown

    print("Repeat without parallelism:   ")
    println {
      val t0 = System.nanoTime
      val hold = fibParms map fib
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " seconds"
    }

    println()

  }
}
