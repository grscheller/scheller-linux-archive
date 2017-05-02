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

    val numThreads_A = 350
    val numThreads_B = 700
    val es_A = Executors.newFixedThreadPool(numThreads_A)
    val es_B = Executors.newFixedThreadPool(numThreads_B)

    println("\nTest Par.parFilter:")

    import fpinscala.state.rand.{Rand,RNG,LCG}  

    // First test with something trivial to see if each
    // implementation actually works.
    val shortList = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
    val parNo2or3Mults1 =
      parFilter1(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3 with parFilter1: ")
    println(run(es_A)(parNo2or3Mults1).get)

    val parNo2or3Mults2 =
      parFilter2(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3 with parFilter2: ")
    println(run(es_A)(parNo2or3Mults2).get)

    val parNo2or3Mults3 =
      parFilter3(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3 with parFilter3: ")
    println(run(es_A)(parNo2or3Mults3).get)

    val parNo2or3Mults4 =
      parFilter4(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3 with parFilter4: ")
    println(run(es_A)(parNo2or3Mults4).get)

    val parNo2or3Mults5 =
      parFilter5(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3 with parFilter5: ")
    println(run(es_A)(parNo2or3Mults5).get)

    val parNo2or3Mults =
      parFilter(shortList)((x: Int) => x % 2 != 0 && x % 3 != 0)
    println("\nFilter out multiples of 2 and 3 with parFilter: ")
    println(run(es_A)(parNo2or3Mults).get)

    // Less trivial test - size constrained by thread problem.
    def makeRandomList(lt: Int, len: Int) =
      Rand.sequence(List.fill(len)(RNG.nonNegativeLessThan(lt)))

    val randList = makeRandomList(100, 200)(LCG(234))

    // Test 1:
    print("\nTime parFilter1 with " + numThreads_A + " threads: ")
    val futEvenRandList1_1A = run(es_A)(parFilter1(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList1_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter2 with " + numThreads_A + " threads: ")
    val futEvenRandList2_1A = run(es_A)(parFilter2(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList2_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter3 with " + numThreads_A + " threads: ")
    val futEvenRandList3_1A = run(es_A)(parFilter3(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList3_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter4 with " + numThreads_A + " threads: ")
    val futEvenRandList4_1A = run(es_A)(parFilter4(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList4_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter5 with " + numThreads_A + " threads: ")
    val futEvenRandList5_1A = run(es_A)(parFilter5(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList5_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter with  " + numThreads_A + " threads: ")
    val futEvenRandList_1A = run(es_A)(parFilter(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_1A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Repeat above without parallelism: ")
    println {
      val t0 = System.nanoTime
      val hold = randList filter (_ % 2 == 0)
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    // Test 2:
    print("\nTime parFilter1 with " + numThreads_A + " threads: ")
    val futEvenRandList1_2A = run(es_A)(parFilter1(randList)(_ % 5 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList1_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter2 with " + numThreads_A + " threads: ")
    val futEvenRandList2_2A = run(es_A)(parFilter2(randList)(_ % 5 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList2_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter3 with " + numThreads_A + " threads: ")
    val futEvenRandList3_2A = run(es_A)(parFilter3(randList)(_ % 5 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList3_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter4 with " + numThreads_A + " threads: ")
    val futEvenRandList4_2A = run(es_A)(parFilter4(randList)(_ % 5 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList4_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter5 with " + numThreads_A + " threads: ")
    val futEvenRandList5_2A = run(es_A)(parFilter5(randList)(_ % 5 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList5_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter  with " + numThreads_A + " threads: ")
    val futEvenRandList_2A = run(es_A)(parFilter(randList)(_ % 5 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_2A.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Repeat above without parallelism: ")
    println {
      val t0 = System.nanoTime
      val hold = randList filter (_ % 5 == 0)
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    // Test 3:
    print("\nTime parFilter1 with " + numThreads_B + " threads: ")
    val futEvenRandList1_3B = run(es_B)(parFilter1(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList1_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter2 with " + numThreads_B + " threads: ")
    val futEvenRandList2_3B = run(es_B)(parFilter2(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList2_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter3 with " + numThreads_B + " threads: ")
    val futEvenRandList3_3B = run(es_B)(parFilter3(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList3_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter4 with " + numThreads_B + " threads: ")
    val futEvenRandList4_3B = run(es_B)(parFilter4(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList4_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter5 with " + numThreads_B + " threads: ")
    val futEvenRandList5_3B = run(es_B)(parFilter5(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList5_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter  with " + numThreads_B + " threads: ")
    val futEvenRandList_3B = run(es_B)(parFilter(randList)(_ % 2 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_3B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Repeat above without parallelism: ")
    println {
      val t0 = System.nanoTime
      val hold = randList filter (_ % 2 == 0)
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    // Test 4:
    print("\nTime parFilter1 with " + numThreads_B + " threads: ")
    val futEvenRandList1_4B = run(es_B)(parFilter1(randList)(_ % 3 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList1_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter2 with " + numThreads_B + " threads: ")
    val futEvenRandList2_4B = run(es_B)(parFilter2(randList)(_ % 3 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList2_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter3 with " + numThreads_B + " threads: ")
    val futEvenRandList3_4B = run(es_B)(parFilter3(randList)(_ % 3 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList3_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter4 with " + numThreads_B + " threads: ")
    val futEvenRandList4_4B = run(es_B)(parFilter4(randList)(_ % 3 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList4_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter5 with " + numThreads_B + " threads: ")
    val futEvenRandList5_4B = run(es_B)(parFilter5(randList)(_ % 3 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList5_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Time parFilter  with " + numThreads_B + " threads: ")
    val futEvenRandList_4B = run(es_B)(parFilter(randList)(_ % 3 == 0))
    println {
      val t0 = System.nanoTime
      val hold = futEvenRandList_4B.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    print("Repeat above without parallelism: ")
    println {
      val t0 = System.nanoTime
      val hold = randList filter (_ % 3 == 0)
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000.0 + " μs."
    }

    es_A.shutdown
    es_B.shutdown

    // Test with longer calculations.

    val numThreads_fib = 80
    val es_fib = Executors.newFixedThreadPool(numThreads_fib)

    val fibParms = List.range(0,46) map (_.toLong)

    print("\nTime parMap1 with " + numThreads_fib + " threads: ")
    val futFib1 = run(es_fib)(parMap1(fibParms)(fib))
    println {
      val t0 = System.nanoTime
      val hold = futFib1.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " s."
    }

    print("Time parMap2 with " + numThreads_fib + " threads: ")
    val futFib2 = run(es_fib)(parMap2(fibParms)(fib))
    println {
      val t0 = System.nanoTime
      val hold = futFib2.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " s."
    }

    print("Time parMap3 with " + numThreads_fib + " threads: ")
    val futFib3 = run(es_fib)(parMap3(fibParms)(fib))
    println {
      val t0 = System.nanoTime
      val hold = futFib3.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " s."
    }

    print("Time parMap4 with " + numThreads_fib + " threads: ")
    val futFib4 = run(es_fib)(parMap4(fibParms)(fib))
    println {
      val t0 = System.nanoTime
      val hold = futFib4.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " s."
    }

    print("Time parMap5 with " + numThreads_fib + " threads: ")
    val futFib5 = run(es_fib)(parMap5(fibParms)(fib))
    println {
      val t0 = System.nanoTime
      val hold = futFib5.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " s."
    }

    print("Time parMap  with " + numThreads_fib + " threads: ")
    val futFib = run(es_fib)(parMap(fibParms)(fib))
    println {
      val t0 = System.nanoTime
      val hold = futFib.get
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " s."
    }

    es_fib.shutdown

    print("Repeat without parallelism:   ")
    println {
      val t0 = System.nanoTime
      val hold = fibParms map fib
      val t1 = System.nanoTime
      hold.length + " in " + (t1 - t0)/1000000000.0 + " s."
    }

    println()

  }
}
