package fpinscala.chap07.parallelism

import scala.util.{Try, Success, Failure}
import java.util.concurrent._
import fpinscala.parallelism.javaFutures.Par
import Par._

import Par._

/** Test fpinscala.parallelism.javaFutures.Par */
object ParJavaFutures4 {

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(20)

    // Test parMap error handling

    val parDiv42 = (l: List[Int]) => parMap(l)(42/_)

    val nonZero  = List(42, 210, 1, 20, 55, 6, 3, 5, 10, 777, 9)

    println()

    println(Try { parDiv42( nonZero).run(es) })

    println()

    // Test flatMap via choice
    def slowBool(b: Boolean): Boolean = { Thread.sleep(1000); b }    

    val myChoice = choice(asyncF(slowBool)(5 < 7))(
        asyncF(print(_: String))("5 is less than 7 => ")
      , asyncF(print(_: String))("5 is not less than 7 => ")
      )

    println(Try { myChoice.run(es) })

    println()

    es.shutdown

  }

}
