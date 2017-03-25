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

  def main(args: Array[String]): Unit = {

    // Done before exercise 7.3

    val a100 = Par.unit[Int](100)
    val b500 = Par.unit(500: Int)
    val  c50 = Par.unit(50)

    val ab = Par.map2(a100, b500)(_ + _)
    val abc = Par.fork((Par.map2(ab, c50)(_ - _)))

    val es = Executors.newFixedThreadPool(4)
    val abcFuture = Par.run(es)(abc)

    println("\n(100 + 500) - 50 = " + abcFuture.get)

    es.shutdown

    println()

  }

}
