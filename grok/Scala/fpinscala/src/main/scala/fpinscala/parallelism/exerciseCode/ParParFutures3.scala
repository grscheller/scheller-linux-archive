package fpinscala.chap07.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import fpinscala.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Blocking object
 *
 *  Test parallel calculations with simple calculations
 *  on larger data structures.
 *
 */
object ParParFutures3 {

  def timePar[A](par: Par[A], es: ExecutorService, comment: String): Unit =
    println {
      print(comment)
      val t0 = System.nanoTime
      val hold = par.run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds"
    }

  def timeIt[A,B](it: A => B, arg: A, comment: String): Unit =
    println {
      print(comment)
      val t0 = System.nanoTime
      val hold = it(arg)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds"
    }

  /** Parse input args to determine scale of datastructure */
  def parseArgs(args: Array[String]): Long =
    if (args.length == 0) 100
    else args(0).toLong

  def main(args: Array[String]): Unit = {

    var es: ExecutorService = null

    val numThreads = List(50, 20, 10, 5, 4, 3, 2, 1)

    val dataList = List.range[Long](1, parseArgs(args))
    println(s"\nCreated a list from 1 to ${dataList.length}.")

    // contrived busy work - not a good canidate for parallel calculation
    val parSumMod42Evens = (l1: List[Long]) => 
      join(parMap(l1)(_ % 42) map {
        (l2: List[Long]) => parFilter(l2)(_ % 2 == 0)
      }) map {
        (l3: List[Long]) => l3.foldLeft(0L)(_ + _)
      }

    val sumMod42Evens = (l: List[Long]) =>
      l.map(_ % 42).filter(_ % 2 == 0).foldLeft(0L)(_ + _)

    timeIt(sumMod42Evens, dataList, "No parallelism: ")

    numThreads foreach {
      threadNum => {
        es = Executors.newFixedThreadPool(threadNum)
        timePar( parSumMod42Evens(dataList)
               , es
               , s"Parallel with ${threadNum} threads: " )
        es.shutdown
      }
    }

    // Longer running parallel calculations
    def slowAdd(a: Int, b: Int) = {
      Thread.sleep(1000)
      a + b
    }

    def slowInc(a: Int) = {
      Thread.sleep(1000)
      a + 1
    }

    // serial calculation

    // paralllel calculation
    val modestVec = Vector(0,1,2,3,4,5,6,7,8,9)
    val vecPars = modestVec map asyncF(slowInc)
    val par55 = balancedBinComp(vecPars)(slowAdd)

    // serialize above
    def serially(vec: Vector[Int]) =
      vec.map(slowInc(_)).foldLeft(0)(slowAdd(_, _))

    timeIt(serially, modestVec, "\nNo parallelism: ")

    numThreads foreach {
      threadNum => {
        es = Executors.newFixedThreadPool(threadNum)
        timePar(par55, es , s"Parallel with ${threadNum} threads: ")
        es.shutdown
      }
    }

    println()

  }

}
