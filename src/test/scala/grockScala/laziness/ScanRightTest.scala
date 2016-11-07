package grockScala.chap05.laziness

import grockScala.laziness._
import grockScala.laziness.Stream._

object ScanRightTest {

  // Some test data
  val fiveToOne = range(5, 0)

  def main(args: Array[String]): Unit = {

    // Test scanRight commutivite vs anticommunitive function
    print("fiveToOne = "); println(fiveToOne.toList)

    print("\nfiveToOne.scanRight(0)(_ + _).toList = ")
    println(fiveToOne.scanRight(0)(_ + _).toList)

    print("\nfiveToOne.scanRight(0)(_ - _).toList = ")
    println(fiveToOne.scanRight(0)(_ - _).toList)

  }

}
