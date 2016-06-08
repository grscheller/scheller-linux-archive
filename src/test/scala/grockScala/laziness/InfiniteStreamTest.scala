package grockScala.test.laziness

import grockScala.laziness._

object InfiniteStreamTest{

  // Infinite data structure - infinite Stream of 42's
  val fortyTwos: Stream[Int] = Stream.cons(42, fortyTwos)

  /*
     Like in main method, this does not work here.
  */
  // def six(): Int = {
  //   val ones: Stream[Int] = Stream.cons(1, ones)
  //   ones.take(6).foldRight(0)(_ + _)
  // }

  /*
     This works, modeled after constant method.
  */
  def six(): Int = {
    lazy val ones: Stream[Int] = Cons(() => 1, () => ones)
    ones.take(6).foldRight(0)(_ + _)
  }

  // This works.
  def oneSixtyEight(): Int = {
    fortyTwos.take(4).foldRight(0)(_ + _)
  }

  // My original version of constant
  def constant1[A](a: A): Stream[A] =
    Stream.cons(a, constant1(a))

  // Books version - more efficient since it is one
  //                 object referencing itself.
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /** Create an infinite stream of the Fibonaccii numbers
   *
   *    Make them Longs, so they are more useful.
   */
  def fibs(): Stream[Long] = fibStream(0L, 1L)

  def fibStream(f0: Long, f1: Long): Stream[Long] =
    Stream.cons(f0, fibStream(f1, f0 + f1))

  def fibs_unfold(): Stream[Long] = {
    def f(fp: (Long, Long)) = {
      val f1 = fp._1
      val f2 = fp._2
      if (f1 >= 0L) Some((f1, (f2, f1 + f2)))
      else None
    }
    Stream.unfold((0L, 1L))(f)
  }

  def fibs_unfold1(): Stream[Long] = {
    def f(fp: (Long, Long)) = {
      val f1 = fp._1
      val f2 = fp._2
      if (f1 >= 0L) Some((f1, (f2, f1 + f2)))
      else None
    }
    Stream.unfold1((0L, 1L))(f)
  }

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
       Also, fails this way when adjusted to use
       scala.collection.immutable.Stream.
    */
    // val ones: Stream[Int] = Stream.cons(1, ones)
    // print("ones.drop(42).take(10).headOption = ")
    // println(ones.drop(42).take(10).headOption)

    /*
       This also does not work here.
    */
    // def five(): Int = {
    //   val ones: Stream[Int] = Stream.cons(1, ones)
    //   ones.take(5).foldRight(0)(_ + _)
    // }
    // print("\nfive() = " + five())

    /*
       But does work using lazy vals.
    */
    def five(): Int = {
      lazy val ones: Stream[Int] = Cons(() => 1, () => ones)
      ones.take(5).foldRight(0)(_ + _)
    }
    println("five() = " + five())

    // Also works as a method:
    println("six() = " + six())

    // As does this:
    println("oneSixtyEight() = " + oneSixtyEight())

    // Finally, works but subject to stackover flow
    lazy val twos: Stream[Int] = Cons(() => 2, () => twos)
    print("twos.take(10).drop(3).toList = ")
    println(twos.take(10).drop(3).toList)
    print("twos.take(1000).drop(993).toList = ")
    println(twos.take(1000).drop(993).toList)
    // Stackoverflow (None if using scala.collection.immutable.Stream)
    // print("twos.take(10000).drop(9993).toList = ")
    // println(twos.take(10000).drop(9993).toList)
    // print("twos.drop(10000).take(7).toList = ")
    // println(twos.drop(10000).take(7).toList)

    // Test constant method

    println("\nTest constant methods:")
    print("constant1(7).drop(42).take(9).toList = ")
    println(constant1(7).drop(42).take(9).toList)
    print("constant(9).drop(42).take(7).toList = ")
    println(constant(9).drop(42).take(7).toList)
    // The drop is more prone to make expression sensitive to
    // stackoverflow than the take because all the values need
    // to actually be dropped, but not taken.
    print("constant1(7).take(800000).drop(4200).take(9).toList = ")
    println(constant1(7).take(800000).drop(4200).take(9).toList)
    print("constant(9).take(800000).drop(4200).take(7).toList = ")
    println(constant(9).take(800000).drop(4200).take(7).toList)

    // Test from method

    println("\nTest from methods:")

    val count42to52 = Stream.from(42).takeWhile(_ < 53)
    println("count42to52.toList = " + count42to52.toList)

    val oneTo1000 = Stream.from(1).takeWhile(_ <= 1000)
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

    // Compare my unfold method with books unfold method

    println("\nCompare two unfold implementations -")
    println("First with Fibonacci streams:")
    println("fibs_unfold.toList =  " + fibs_unfold.toList + "\n")
    println("fibs_unfold1.toList = " + fibs_unfold1.toList + "\n")

    println()

  }
}
