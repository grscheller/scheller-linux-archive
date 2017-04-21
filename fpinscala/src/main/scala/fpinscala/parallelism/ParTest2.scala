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

    val es = Executors.newFixedThreadPool(4)

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

    // Test Par.sequence
    println("\nTest Par.sequence:")
    
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

    // Test Par.parFilter
    println("\nTest Par.parFilter:")

    import fpinscala.state.rand.{Rand,RNG,LCG}  

    val shortList = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
    val parNo2or3Mults =
      parFilter(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3: ")
    println(run(es)(parNo2or3Mults).get)

    def makeRandomList(lt: Int, len: Int) =
      Rand.sequence(List.fill(len)(RNG.nonNegativeLessThan(lt)))

    println("\nFilter out odds from a list of non-neg random Ints < 100:")
    val randList = makeRandomList(100, 1000)(LCG(234))
    val futEvenRandList = run(es)(parFilter(randList)(_ % 2 == 0))
    val evenRandList = futEvenRandList.get
    println(evenRandList)
    print("\nrandList.length = ")
    println(randList.length)
    print("evenRandList.length = ")
    println(evenRandList.length)

    es.shutdown

    println()

  }
}
