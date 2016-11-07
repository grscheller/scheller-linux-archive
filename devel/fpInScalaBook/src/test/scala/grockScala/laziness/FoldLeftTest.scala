package grockScala.chap05.laziness

import grockScala.laziness._
import grockScala.laziness.Stream._

object FoldLeftTest {

  // Some test data
  val oneToFive = range(1, 6)

  def main(args: Array[String]): Unit = {

    // foldRight vs foldLeft communitive function
    print("\noneToFive = "); println(oneToFive.toList)

    print("\noneToFive.foldRight(0)(_ + _) = ")
    println(oneToFive.foldRight(0)(_ + _))

    print("\noneToFive.foldLeft(0)(_ + _)  = ")
    println(oneToFive.foldLeft(0)(_ + _))

  }

}
