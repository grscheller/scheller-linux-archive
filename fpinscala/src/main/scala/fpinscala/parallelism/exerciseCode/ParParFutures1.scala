package fpinscala.chap07.parallelism

import scala.util.{Try, Success, Failure}
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeoutException
import fpinscala.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Par */
object ParParFutures1 {

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(4)

    // Test parMap error handling

    val parDiv42 = (l: List[Int]) => parMap(l)(42/_)

    val nonZero  = List(42, 210, 1, 20, 55, 6, 3, 5, 10, 777, 9)
    val withZero = List(42, 210, 1, 20, 55, 0, 3, 5, 10, 777, 9)

    println()

    println(Try { parDiv42( nonZero).run(es) })
    println(Try { parDiv42(withZero).run(es) })

    println()

    // Test flatMap via choice
    def slowBool(b: Boolean): Boolean = { Thread.sleep(1000); b }    
    def noBool(b: Boolean): Boolean = { Thread.sleep(1000); throw(new TimeoutException) }    

    val myChoice = choice(asyncF(slowBool)(5 < 7))(
        asyncF(print(_: String))("5 is less than 7 => ")
      , asyncF(print(_: String))("5 is not less than 7 => ")
      )

    val noChoice = choice(asyncF(noBool)(5 < 7))(
        asyncF(print(_: String))("5 is less than 7 => ")
      , asyncF(print(_: String))("5 is not less than 7 => ")
      )

    val badChoice = choice(asyncF(slowBool)(5 < 7))(
        asyncF((_: String) => throw(new Exception("bad choice")))("5 is less than 7 => ")
      , asyncF(print(_: String))("5 is not less than => 7")
      )

    val luckyChoice = choice(asyncF(slowBool)(5 < 7))(
        asyncF(print(_: String))("5 is less than 7 => ")
      , asyncF((_: String) => throw(new Exception("lucky choice")))("5 is not less than 7 => ")
      )

    println(Try { myChoice.run(es) })
    println(Try { noChoice.run(es) })
    println(Try { badChoice.run(es) })
    println(Try { luckyChoice.run(es) })

    println()

    es.shutdown

  }

}
