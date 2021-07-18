package fpinscala.chap05.laziness

import fpinscala.laziness._
import fpinscala.laziness.Stream._

object scanRightTest {

  // Some test data
  val fiveToOne = range(5, 0)
  val oneTo100 = range(1, 101)

  // tails via scanRight
  def tails3[C](cs: Stream[C]): Stream[Stream[C]] =
    cs.scanRight(empty[C])(cons(_,_))

  def main(args: Array[String]): Unit = {

    print("\nfiveToOne.toList = ")
    println(fiveToOne.toList)

    // Test scanRight commutivite vs anticommunitive function
    println("\nTest scanRight with communitive & anticommunitive function:")

    print("fiveToOne.scanRight(0)(_ + _).toList = ")
    println(fiveToOne.scanRight(0)(_ + _).toList)

    print("fiveToOne.scanRight(0)(_ - _).toList = ")
    println(fiveToOne.scanRight(0)(_ - _).toList)

    // Test tails3 vs tails
    print("\nTest tails3 (via scanRight) vs. ")
    println("Stream.tails: (via unfold)")

    print("(tails3(oneTo100) drop 95).headSafe(empty).toList) = ")
    println((tails3(oneTo100) drop 95).headSafe(empty).toList)
    print("(oneTo100.tails drop 95).headSafe(empty).toList = ")
    println((oneTo100.tails drop 95).headSafe(empty).toList)

    println()
  }

}
