package fpinscala.chap07.parallelism

import java.util.concurrent.Executors
import java.util.concurrent.TimeoutException
import scala.util.{Try, Success, Failure}
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
    def noBool(b: Boolean): Boolean = {
      Thread.sleep(1000)
      throw(new TimeoutException)
    }    

    val myChoice1 = choice(asyncF(slowBool)(5 < 7))(
        asyncF(print(_: String))("5 is less than 7 => ")
      , asyncF(print(_: String))("5 is not less than 7 => ")
      )

    val noChoice1 = choice(asyncF(noBool)(5 < 7))(
        asyncF(print(_: String))("5 is less than 7 => ")
      , asyncF(print(_: String))("5 is not less than 7 => ")
      )

    val badChoice1 = choice(asyncF(slowBool)(5 < 7))(
        asyncF((_: String) => throw(new Exception("bad choice")))("5 is less than 7 => ")
      , asyncF(print(_: String))("5 is not less than => 7")
      )

    val luckyChoice1 = choice(asyncF(slowBool)(5 < 7))(
        asyncF(print(_: String))("5 is less than 7 => ")
      , asyncF((_: String) => throw(new Exception("lucky choice")))("5 is not less than 7 => ")
      )

    println(Try { myChoice1.run(es) })
    println(Try { noChoice1.run(es) })
    println(Try { badChoice1.run(es) })
    println(Try { luckyChoice1.run(es) })

    println()

    val myChoice2 = choice(asyncF(slowBool)(7 < 5))(
        asyncF(print(_: String))("7 is less than 5 => ")
      , asyncF(print(_: String))("7 is not less than 5 => ")
      )

    val noChoice2 = choice(asyncF(noBool)(7 < 5))(
        asyncF(print(_: String))("7 is less than 5 => ")
      , asyncF(print(_: String))("7 is not less than 5 => ")
      )

    val luckyChoice2 = choice(asyncF(slowBool)(7 < 5))(
        asyncF((_: String) => throw(new Exception("lucky choice")))("7 is less than 5 => ")
      , asyncF(print(_: String))("7 is not less than 5 => ")
      )

    val badChoice2 = choice(asyncF(slowBool)(7 < 5))(
        asyncF(print(_: String))("7 is less than 5 => ")
      , asyncF((_: String) => throw(new Exception("bad choice")))("7 is not less than 5 => ")
      )

    println(Try { myChoice2.run(es) })
    println(Try { noChoice2.run(es) })
    println(Try { luckyChoice2.run(es) })
    println(Try { badChoice2.run(es) })

    println()

    es.shutdown

  }

}
