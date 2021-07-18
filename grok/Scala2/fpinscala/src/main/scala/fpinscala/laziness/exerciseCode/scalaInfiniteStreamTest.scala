package fpinscala.chap05.laziness

import scala.collection.immutable.LazyList.{cons, from}

/** Repeat first half of InfiniteStreamTest but use
  *  LazyList class from Scala Collections.
  */
object scalaInfiniteStreamTest {

  // Infinite data structure - infinite LazyList of 42's
  val fortyTwos: LazyList[Int] = cons(42, fortyTwos)

  // This works.
  def oneSixtyEight: Int = {
    fortyTwos.take(4).foldRight(0)(_ + _)
  }

  // My original version of constant
  def constant[A](a: A): LazyList[A] = cons(a, constant(a))

  /** Create an infinite stream of the Fibonaccii numbers
    *
    *    Make them Longs, so they are more useful.
    */
  def fibs: LazyList[Long] = fibStream(0L, 1L)

  def fibStream(f0: Long, f1: Long): LazyList[Long] =
    cons(f0, fibStream(f1, f0 + f1))

  def main(args: Array[String]): Unit = {

    // Need to figure out what exactly works.
    println("\nFirst figure out what works:")

    print("fortyTwos.drop(42).take(42).headOption = ")
    println(fortyTwos.drop(42).take(42).headOption)

    /*
       Does not compile.  Error message is:
         "forward reference extends over
          definition of value ones"
       Fails for both scala-2.12.0-M4 and scala-2.11.7.
     */
    // val ones: LazyList[Int] = cons(1, ones)
    // print("ones.drop(42).take(10).headOption = ")
    // println(ones.drop(42).take(10).headOption)

    // Works if we make the val lazy
    lazy val twos: LazyList[Int] = cons(2, twos)
    print("twos.take(10).drop(3).toList = ")
    println(twos.take(10).drop(3).toList)
    print("twos.take(1000).drop(993).toList = ")
    println(twos.take(1000).drop(993).toList)
    print("twos.take(1000000).drop(999993).toList = ")
    println(twos.take(1000000).drop(999993).toList)
    print("twos.drop(10000000).take(7).toList = ")
    println(twos.drop(10000000).take(7).toList)

    /*
       This works if we use a lazy val.
     */
    def six: Int = {
      lazy val ones: LazyList[Int] = cons(1, ones)
      ones.take(6).foldRight(0)(_ + _)
    }
    println("six = " + six)

    // As does this:
    println("oneSixtyEight = " + oneSixtyEight)

    // Test constant method

    println("\nTest constant methods:")
    print("constant(9).drop(42).take(7).toList = ")
    println(constant(9).drop(42).take(7).toList)
    print("constant(9).take(800000).drop(420000).take(7).toList = ")
    println(constant(9).take(800000).drop(420000).take(7).toList)

    // Test from method

    println("\nTest from methods:")

    val count42to52 = from(42).takeWhile(_ < 53)
    println("count42to52.toList = " + count42to52.toList)

    val oneTo1000 = from(1).takeWhile(_ <= 1000)
    var sumAccm = 0
    for (nn <- oneTo1000) sumAccm += nn
    println("\nSum 1 to 1000 is " + sumAccm)
    println("Sum 1 to 1000 is " + oneTo1000.foldRight(0)(_ + _))

    // Test Fibonaccii Stream
    println("\nPrint the first 100 Fibonaccii numbers:")
    for (fib <- fibs.take(100)) println(fib)
    println("Last 7 values are anomalous due to Long rollover.")

    println("\nFibonaccii numbers run negative too:")
    for (fib <- fibStream(34, -21).take(20)) println(fib)

    /* Only could atart with -3000 reliably, -3700 intermitantly
       with grockscala versions before stack overflow. */
    print("\nCompare grockscala.laziness.{from,dropWhile}:")
    print("\nfrom(-20000).dropWhile(_ < 1600).take(3).toList = ")
    println(from(-20000).dropWhile(_ < 1600).take(3).toList)
    println()

  }
}
