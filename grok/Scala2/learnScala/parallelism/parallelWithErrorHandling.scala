package grokScala.parallelism

import scala.util.{Try, Success, Failure}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/** Using Scala Futures circa 3rd edition Odersky's book.  */
object HandleErrors {

  def divInto42AndSum(l: List[Int]): Future[Int] =
    Future { l.map(42/_).foldLeft(0)(_ + _) }

  def main(args: Array[String]): Unit = {

    val nonZero  = List(42, 210, 1, 20, 55, 6, 3, 5, 10, 777, 9)
    val withZero = List(42, 210, 1, 20, 55, 0, 3, 5, 10, 777, 9)

    val shouldWork = divInto42AndSum(nonZero)
    val shouldFail = divInto42AndSum(withZero)

    // Make sure we are done.
    Thread.sleep(1000)

    println()

    println(shouldWork.value)
    println(shouldFail.value)

    println()

  }

}
