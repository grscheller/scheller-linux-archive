package fpinscala.chap07.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
//import fpinscala.parallelism.javaFutures.Par
import fpinscala.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Blocking object
 *
 *  Before section 7.4
 *
 */
object Sleepy {

  def timePar[A](par: Par[A], es: ExecutorService, comment: String): Unit =
    println {
      print(comment)
      val t0 = System.nanoTime
      val hold = par.run(es)
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds"
    }

  def timeIt[A,B](it: A => B, arg: A, comment: String): Unit =
    println {
      print(comment)
      val t0 = System.nanoTime
      val hold = it(arg)
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds"
    }

  def main(args: Array[String]): Unit = {

    val threadNum1 = 40
    val threadNum2 = 60
    val threadNum3 = 80

    val es1 = Executors.newFixedThreadPool(threadNum1)
    val es2 = Executors.newFixedThreadPool(threadNum2)
    val es3 = Executors.newFixedThreadPool(threadNum3)

    val myList = List[Int](42, 1, 69, 12, 17, 29, 76, 21, 26, 121, 21, 100,
                           45, 99, 86, 37, 13, 4, 37, 72, 9,  53, 103,  17,
                           21, 164, 14, 173, 18, 12,   1, 67, 11,  52, 113,
                           23, 45, 118,  11, 82,  3,  12, 65, 12,  19, 45 )

    val sleepy = (m: Int) => {
      Thread.sleep(m * 10) // nap time
      m
    }

    val parSleep = parMap(myList)(sleepy) map {_.foldLeft(0)(_ + _)}
    val deepSleep = (l: List[Int]) => (l map sleepy).foldLeft(0)(_ + _)

    println()

    timePar(parSleep, es1, s"Threaded sleep with ${threadNum1} threads: ")
    timePar(parSleep, es2, s"Threaded sleep with ${threadNum2} threads: ")
    timePar(parSleep, es3, s"Threaded sleep with ${threadNum3} threads: ")
    timeIt(deepSleep, myList, "Serial sleep: ")
  
    es1.shutdown
    es2.shutdown
    es3.shutdown

    println()

  }

}
