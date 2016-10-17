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

  // Using unfold method for constant
  def constantU[A](a: A): Stream[A] = 
    Stream.unfold(a)((a0: A) => Some((a0, a0)))

  // Using unfold method for constant - Book version
  def constantU_Book[A](a: A): Stream[A] = 
    Stream.unfold(a)(_ => Some((a, a)))

  // Using unfold method for constant - Testing Syntax
  def constantU_Book_version2[A](a: A): Stream[A] = 
    Stream.unfold(a) {case _ => Some((a, a))}

  // Using unfold method for constant - Testing Syntax
  def constantU_Book_version3[A](a: A): Stream[A] = 
    Stream.unfold(a) { _ => Some((a, a)) }

  /** Create an infinite stream of the Fibonaccii numbers
   *
   *    Make them Longs, so they are more useful.
   */
  def fibs(): Stream[Long] = fibStream(0L, 1L)

  def fibStream(f0: Long, f1: Long): Stream[Long] =
    Stream.cons(f0, fibStream(f1, f0 + f1))

  def fibs_unfold1(): Stream[Long] = {
    def f(fp: (Long, Long)) = {
      val f1 = fp._1
      val f2 = fp._2
      if (f1 >= 0L) Some((f1, (f2, f1 + f2)))
      else None
    }
    Stream.unfold1((0L, 1L))(f)
  }

  // Similar to above (if I used a case statement instead of the
  // if statement), except using a syntactic shortcut Scala
  // provides when you create a variable just to match on it.
  def fibs_unfold(): Stream[Long] =
    Stream.unfold((0L, 1L)) {
      case (f1, f2) if f1 >= 0 => Some((f1, (f2, f1 + f2)))
      case _ => None
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
    print("constant1(7).drop(42).take(7).toList = ")
    println(constant1(7).drop(42).take(7).toList)
    print("constant(8).drop(42).take(8).toList = ")
    println(constant(8).drop(42).take(8).toList)
    print("constantU(9).drop(42).take(9).toList = ")
    println(constantU(9).drop(42).take(9).toList)
    // The drop is more prone to make expression sensitive to
    // stackoverflow than the take because all the values need
    // to actually be dropped, but not taken.
    print("constant1(7).take(800000).drop(4200).take(7).toList = ")
    println(constant1(7).take(800000).drop(4200).take(7).toList)
    print("constant(8).take(800000).drop(4200).take(8).toList = ")
    println(constant(8).take(800000).drop(4200).take(8).toList)
    print("constantU(9).take(800000).drop(4200).take(9).toList = ")
    println(constantU(9).take(800000).drop(4200).take(9).toList)

    // Test constant methods I moved to Stream companion object

    println("\nTest constant methods:")
    print("Stream.const1(7).drop(42).take(7).toList = ")
    println(Stream.const1(7).drop(42).take(7).toList)
    print("Stream.const(8).drop(42).take(8).toList = ")
    println(Stream.const(8).drop(42).take(8).toList)
    print("Stream.constU(9).drop(42).take(9).toList = ")
    println(Stream.constU(9).drop(42).take(9).toList)
    print("Stream.const1(7).take(800000).drop(4200).take(7).toList = ")
    println(Stream.const1(7).take(800000).drop(4200).take(7).toList)
    print("Stream.const(8).take(800000).drop(4200).take(8).toList = ")
    println(Stream.const(8).take(800000).drop(4200).take(8).toList)
    print("Stream.constU(9).take(800000).drop(4200).take(9).toList = ")
    println(Stream.constU(9).take(800000).drop(4200).take(9).toList)

    // Test from and range methods

    val count42to52 = Stream.from(42).takeWhile(_ < 53)
    println("\ncount42to52.toList = " + count42to52.toList)

    val oneTo1000 = Stream.from(1).takeWhile(_ <= 1000)
    var sumAccm = 0
    for (nn <- oneTo1000) sumAccm += nn
    println("\nSum 1 to 1000 is " + sumAccm)
    println("Sum 1 to 1000 is " + oneTo1000.foldRight(0)(_ + _) + "\n")

    val countDown100 = Stream.range(100, -1)
    val mySin = for {
      ii <- countDown100
      if ii < 51
      x = math.Pi*ii/100.0
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
    print("\nStream.from(-2000).dropWhile(_ < 1600).take(3).toList = ")
    println(Stream.from(-2000).dropWhile(_ < 1600).take(3).toList)
    print("\nStream.fromu(-2000).dropWhile(_ < 1600).take(3).toList = ")
    println(Stream.fromu(-2000).dropWhile(_ < 1600).take(3).toList)
    print("\nStream.fromu1(-2000).dropWhile(_ < 1600).take(3).toList = ")
    println(Stream.fromu1(-2000).dropWhile(_ < 1600).take(3).toList)

    // Test map and mapFR directly
    val cycle7UF = Stream.from(0) map (_ % 7)
    val cycle11FR = Stream.from(0) mapFR (_ % 11)

    println("\nTest map and mapFR directly:")
    print("(cycle7UF take 120 drop 100).toList = ")
    println((cycle7UF take 120 drop 100).toList)
    print("(cycle11FR take 120 drop 100).toList = ")
    println((cycle11FR take 120 drop 100).toList)

    print("(cycle7UF take 20020 drop 20000).toList = ")
    println((cycle7UF take 20020 drop 20000).toList)
    print("(cycle11FR take 20020 drop 20000).toList = ")
    println((cycle11FR take 20020 drop 20000).toList)

    // Test find
    println("\nTest find method:")
    print("Stream.from(43).find(x => x > 1000 && ")
    print("x % 17 == 0 && x % 19 == 0) = ")
    println(Stream.from(42).find(x => x > 1000 && x % 17 == 0 && x % 19 == 0))

    print("Stream.range(42,2042).find(x => x % 131 == 0 && ")
    print("x % 17 == 0 && x % 19 == 0) = ")
    println(Stream.range(42, 2042) find (x => x % 131 == 0 && 
      x % 17 == 0 && x % 19 == 0))

    // Test zipWith and zipAll
    println("\nTest zipWith method:")
    val func = (x: Int, y: Int) => x + 2*y

    print("Stream.range(0,100,10).zipWith(Stream.range(1,6))(func).toList = ")
    println(Stream.range(0,100,10).zipWith(Stream.range(1,6))(func).toList)

    print("Stream.empty[Int].zipWith(Stream.range(1,6))(func).toList = ")
    println(Stream.empty[Int].zipWith(Stream.range(1,6))(func).toList)

    print("Stream.range(0,100,10).zipWith(Stream.empty[Int])(func).toList = ")
    println(Stream.range(0,100,10).zipWith(Stream.empty[Int])(func).toList)

    print("Stream.empty[Int].zipWith(Stream.empty[Int])(func).toList = ")
    println(Stream.empty[Int].zipWith(Stream.empty[Int])(func).toList)

    println("\nTest zipAll method:")

    print("(Stream.range(0,100,10) zipAll Stream.range(1,6)).toList = ")
    println((Stream.range(0,100,10) zipAll Stream.range(1,6)).toList)

    println()

  }
}
