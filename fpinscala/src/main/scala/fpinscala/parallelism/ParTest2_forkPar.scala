package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Par object
 *
 *  Started after exercise 7.4
 */
object ParTest2forkPar {

  // Model an expensive calculations with
  // a less than stellar implementations
  // of the factorial functon.
  def fib(n: Long): Long =
    if (n < 2) n else fib(n-1) + fib(n-2)

  def main(args: Array[String]): Unit = {

    // Test Par.parFilter

    val numThreads_A = 210
    val numThreads_B = 1000
    val es_A = Executors.newFixedThreadPool(numThreads_A)
    val es_B = Executors.newFixedThreadPool(numThreads_B)

    println("\nTest Par.parFilter:")

    import fpinscala.state.rand.{Rand,RNG,LCG}  

    val shortList = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
    val parNo2or3Mults =
      parFilter(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3 with parFilter: ")
    println(run(es_A)(parNo2or3Mults).get)

    val parNo2or3Mults1 =
      parFilter1(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3 with parFilter1: ")
    println(run(es_A)(parNo2or3Mults1).get)

    def makeRandomList(lt: Int, len: Int) =
      Rand.sequence(List.fill(len)(RNG.nonNegativeLessThan(lt)))

    val randList = makeRandomList(100, 200)(LCG(234))

    println("\nTime parFilter with with " + numThreads_A + " threads:")
    val futEvenRandList_1A = run(es_A)(parFilter(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println("Time parFilter1 with with " + numThreads_A + " threads:")
    val futEvenRandList1_1A = run(es_A)(parFilter1(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList1_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println("\nTime parFilter with with " + numThreads_A + " threads:")
    val futEvenRandList_2A = run(es_A)(parFilter(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println("Time parFilter1 with with " + numThreads_A + " threads:")
    val futEvenRandList1_2A = run(es_A)(parFilter1(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList1_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println("\nTime parFilter with with " + numThreads_B + " threads:")
    val futEvenRandList_3B = run(es_B)(parFilter(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println("Time parFilter1 with with " + numThreads_B + " threads:")
    val futEvenRandList1_3B = run(es_B)(parFilter1(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList1_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println("\nTime parFilter with with " + numThreads_B + " threads:")
    val futEvenRandList_4B = run(es_B)(parFilter(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println("Time parFilter1 with with " + numThreads_B + " threads:")
    val futEvenRandList1_4B = run(es_B)(parFilter1(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList1_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    es_A.shutdown
    es_B.shutdown

    println()

  }
}
