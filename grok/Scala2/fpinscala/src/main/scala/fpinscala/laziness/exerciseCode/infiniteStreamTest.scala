package fpinscala.chap05.laziness

import fpinscala.laziness._
import fpinscala.laziness.Stream._

object infiniteStreamTest {

  // Infinite data structure - infinite Stream of 42's
  val fortyTwos: Stream[Int] = cons(42, fortyTwos)

  /*
     Like in main method, this does not work here.
   */
  // def six: Int = {
  //   val ones: Stream[Int] = cons(1, ones)
  //   ones.take(6).foldRight(0)(_ + _)
  // }

  /*
     This works, modeled after constant method.
   */
  def six: Int = {
    lazy val ones: Stream[Int] = Cons(() => 1, () => ones)
    ones.take(6).foldRight(0)(_ + _)
  }

  // This works.
  def oneSixtyEight: Int = {
    fortyTwos.take(4).foldRight(0)(_ + _)
  }

  // My original version of constant
  //   Seems to be more prone to stack overflow
  //   than constant and constantU
  def constant1[A](a: A): Stream[A] = cons(a, constant1(a))

  // Books version - more efficient since it is one
  //                 object referencing a thunk to itself.
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Using unfold method for constant
  def constantU[A](a: A): Stream[A] =
    unfold(a)((a0: A) => Some((a0, a0)))

  // Using unfold method for constant - Book version
  def constantU_Book[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  // Using unfold method for constant - Testing Syntax
  def constantU_Book_version2[A](a: A): Stream[A] =
    unfold(a) { case _ => Some((a, a)) }

  // Using unfold method for constant - Testing Syntax
  def constantU_Book_version3[A](a: A): Stream[A] =
    unfold(a) { _ => Some((a, a)) }

  /** Create an infinite stream of the Fibonaccii numbers
    *
    *    Make them Longs, so they are more useful.
    */
  def fibs: Stream[Long] = fibStream(0L, 1L)

  def fibStream(f0: Long, f1: Long): Stream[Long] =
    cons(f0, fibStream(f1, f0 + f1))

  def fibs_unfold1: Stream[Long] = {
    def f(fp: (Long, Long)) = {
      val f1 = fp._1
      val f2 = fp._2
      if (f1 >= 0L) Some((f1, (f2, f1 + f2)))
      else None
    }
    unfold1((0L, 1L))(f)
  }

  // Similar to above (if I used a case statement instead of the
  // if statement), except using a syntactic shortcut Scala
  // provides when you create a variable just to match on it.
  def fibs_unfold: Stream[Long] =
    unfold((0L, 1L)) {
      case (f1, f2) if f1 >= 0 => Some((f1, (f2, f1 + f2)))
      case _                   => None
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
    def five: Int = {
      lazy val ones: Stream[Int] = Cons(() => 1, () => ones)
      ones.take(5).foldRight(0)(_ + _)
    }
    println("five = " + five)

    // Also works as a method:
    println("six = " + six)

    // As does this:
    println("oneSixtyEight = " + oneSixtyEight)

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

    // Test constant method defined locally

    println("\nTest constant methods defined locally:")
    print("constant1(7).drop(42).take(7).toList = ")
    println(constant1(7).drop(42).take(7).toList)
    print("constant(8).drop(42).take(8).toList = ")
    println(constant(8).drop(42).take(8).toList)
    print("constantU(9).drop(42).take(9).toList = ")
    println(constantU(9).drop(42).take(9).toList)
    // The drop is more prone to make expression sensitive to
    // stackoverflow than the take because all the values need
    // to actually be dropped, but not taken.
    print("constant1(7).take(80000).drop(420).take(7).toList = ")
    println(constant1(7).take(80000).drop(420).take(7).toList)
    print("constant(8).take(80000).drop(420).take(8).toList = ")
    println(constant(8).take(80000).drop(420).take(8).toList)
    print("constantU(9).take(80000).drop(420).take(9).toList = ")
    println(constantU(9).take(80000).drop(420).take(9).toList)

    // Test constant methods I copied to Stream companion object

    println("\nTest constant methods from Stream companion object:")
    print("const1(7).drop(42).take(7).toList = ")
    println(const1(7).drop(42).take(7).toList)
    print("const(8).drop(42).take(8).toList = ")
    println(const(8).drop(42).take(8).toList)
    print("constU(9).drop(42).take(9).toList = ")
    println(constU(9).drop(42).take(9).toList)
    print("const1(7).take(80000).drop(420).take(7).toList = ")
    println(const1(7).take(80000).drop(420).take(7).toList)
    print("const(8).take(80000).drop(420).take(8).toList = ")
    println(const(8).take(80000).drop(420).take(8).toList)
    print("constU(9).take(80000).drop(420).take(9).toList = ")
    println(constU(9).take(80000).drop(420).take(9).toList)

    // Test from and range methods

    val count42to52 = from(42).takeWhile(_ < 53)
    println("\ncount42to52.toList = " + count42to52.toList)

    val oneTo200 = from(1).takeWhile(_ <= 200)
    var sumAccm = 0
    for (nn <- oneTo200) sumAccm += nn
    println("\nSum 1 to 200 is " + sumAccm)
    println("Sum 1 to 200 is " + oneTo200.foldRight(0)(_ + _) + "\n")

    val countDown100 = range(100, -1)
    val mySin = for {
      ii <- countDown100
      if ii < 51
      x = math.Pi * ii / 100.0
    } yield (x, math.sin(x))
    for ((x, y) <- mySin) printf("sin(%f) = %f\n", x, y)

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

    print("Next with from:")
    print("\nfrom(-1000).dropWhile(_ < 600).take(3).toList = ")
    println(from(-1000).dropWhile(_ < 600).take(3).toList)
    print("\nfromu(-1000).dropWhile(_ < 600).take(3).toList = ")
    println(fromu(-1000).dropWhile(_ < 600).take(3).toList)
    print("\nfromu1(-1000).dropWhile(_ < 600).take(3).toList = ")
    println(fromu1(-1000).dropWhile(_ < 600).take(3).toList)

    // Test map and mapFR directly
    val cycle7UF = from(0) map (_ % 7)
    val cycle11FR = from(0) mapFR (_ % 11)

    println("\nTest map and mapFR directly:")
    print("(cycle7UF take 120 drop 100).toList = ")
    println((cycle7UF take 120 drop 100).toList)
    print("(cycle11FR take 120 drop 100).toList = ")
    println((cycle11FR take 120 drop 100).toList)

    print("(cycle7UF take 2020 drop 2000).toList = ")
    println((cycle7UF take 2020 drop 2000).toList)
    print("(cycle11FR take 2020 drop 2000).toList = ")
    println((cycle11FR take 2020 drop 2000).toList)

    // Test find
    println("\nTest find method:")
    print("from(43).find(x => x > 1000 && ")
    print("x % 17 == 0 && x % 19 == 0) = ")
    println(from(42).find(x => x > 1000 && x % 17 == 0 && x % 19 == 0))

    print("range(42,1042).find(x => x % 131 == 0 && ")
    print("x % 17 == 0 && x % 19 == 0) = ")
    println(
      range(42, 1042) find (x =>
        x % 131 == 0 &&
          x % 17 == 0 && x % 19 == 0
      )
    )

    // Test zipWith and zipAll
    println("\nTest zipWith method:")
    val func = (x: Int, y: Int) => x + 2 * y

    print("range(0,100,10).zipWith(range(1,6))(func).toList = ")
    println(range(0, 100, 10).zipWith(range(1, 6))(func).toList)

    print("empty[Int].zipWith(range(1,6))(func).toList = ")
    println(empty[Int].zipWith(range(1, 6))(func).toList)

    print("range(0,100,10).zipWith(empty[Int])(func).toList = ")
    println(range(0, 100, 10).zipWith(empty[Int])(func).toList)

    print("empty[Int].zipWith(empty[Int])(func).toList = ")
    println(empty[Int].zipWith(empty[Int])(func).toList)

    println("\nTest zipAll method:")

    print("(range(0,100,10) zipAll range(1,6)).toList = ")
    println((range(0, 100, 10) zipAll range(1, 6)).toList)

    println()

  }
}
