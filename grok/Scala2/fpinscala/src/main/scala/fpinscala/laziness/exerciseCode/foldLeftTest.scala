package fpinscala.chap05.laziness

import fpinscala.laziness._
import fpinscala.laziness.Stream._

object foldLeftTest {

  // Some test data
  val oneTo100 = range(1, 101)

  def main(args: Array[String]): Unit = {

    // foldRight vs foldLeft communitive function
    print("\noneTo100 = ")
    println(oneTo100.toList)

    print("\noneTo100.foldRight(0)(_ + _) = ")
    println(oneTo100.foldRight(0)(_ + _))

    print("\noneTo100.foldLeft(0)(_ + _)  = ")
    println(oneTo100.foldLeft(0)(_ + _))

    println()
  }

}
