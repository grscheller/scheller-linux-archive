// Try this fpinscala code to work with
// with real scalaz.concurrent.Actors
package grokScalaz.parallelism

import scala.util.{Try, Success, Failure}
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import grokScalaz.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Blocking object
 *
 *  Before section 7.4
 *
 */
object HandleErrors {

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

    val es = Executors.newFixedThreadPool(4)

    val parDiv42 = (l: List[Int]) => parMap(l)(42/_)

    val nonZero  = List(42, 210, 1, 20, 55, 6, 3, 5, 10, 777, 9)
    val withZero = List(42, 210, 1, 20, 55, 0, 3, 5, 10, 777, 9)

    println()

    println(parDiv42( nonZero).run(es))
    println(parDiv42(withZero).run(es))

    println(Try { parDiv42( nonZero).run(es) })
    println(Try { parDiv42(withZero).run(es) })

    println()

    es.shutdown

  }

}
