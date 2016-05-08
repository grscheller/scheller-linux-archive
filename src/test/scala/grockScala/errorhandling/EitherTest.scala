package grockScala.test.errorhanding

import grockScala.errorhandling._
import grockScala.errorhandling.Either._

object EitherStats {

  // Exercise 4.2 - Implement a variance function via flatMap

  /** Computes the mean of a dataset of Doubles */
  def mean(xs: Seq[Double]): Either[String,Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum/xs.size)

  // Actually clearer to me than the equivalent
  // for comprehension,
  /** Computes the variance of a dataset of Doubles */
  def variance(xs: Seq[Double]): Either[String,Double] = 
    mean(xs) flatMap (m =>
    mean(xs map (x => math.pow((x - m), 2))))

  // Translate outer chain above to a for comprehension.
  //
  //   Note: Only in principle could the second mean
  //         fail.  So, mean's return type prevents me
  //         from putting it in the yield.  I end up
  //         having to "unpack" it so that the yield
  //         can "repack" its value.
  //
  /** Computes the variance of a dataset of Doubles */
  def variance1(xs: Seq[Double]): Either[String,Double] =
    for {
      m <- mean(xs)
      v <- mean(xs map (x => math.pow((x - m), 2)))
    } yield v

  // Direct litteral translation back to functional notation.
  // Done so that I can better understand why I found 
  // translating into for/yield notation so difficult.
  //` 
  //   Note: The identity function at end could be a useful
  //         pattern when translating a monadic chain into
  //         a for comprehension when the last bind in
  //         the chain is a flatmap.
  //
  /** Computes the variance of a dataset of Doubles */
  def variance2(xs: Seq[Double]): Either[String,Double] =
    mean(xs) flatMap (m =>
    mean(xs map (x => math.pow((x - m), 2))) map (v => v))

  // Version using pattern matching
  /** Computes the variance of a dataset of Doubles */
  def variance3(xs: Seq[Double]): Either[String,Double] =
    mean(xs) match {
      case Right(m) =>
        mean(xs.map((x: Double) => math.pow((x - m), 2)))
      case a =>
        a 
    }

}

object EitherParse {

  /** 
   *  Take a list of strings and return an Option of a List
   *  of Doubles if all can be converted.
   */
  def parseDoubles1(ss: List[String]): Either[Exception,List[Double]] =
    sequence(ss map (s => Try(s.toDouble)))

  /** 
   *  Take a list of strings and return an Option of a List
   *  of Doubles if all can be converted.
   */  
  def parseDoubles(ss: List[String]): Either[Exception,List[Double]] =
    traverse(ss)(s => Try(s.toDouble))

  /** 
   *  Take a list of strings and return an Option of a List
   *  of Ints if all can be converted.
   */  
  def parseInts(ss: List[String]): Either[Exception,List[Int]] =
    traverse(ss)(s => Try(s.toInt))

}

object EitherTest {

  import EitherStats._
  import EitherParse._

  // Define some utility functions
  /**
   * Evaluate and nicely print expresion - let any
   * exceptions happen before anything printed.
   */
  def evalP0[A](expr: => A, fname: String): Unit = {
    val result = expr  // Let any exceptions happen before anything printed.
    print(fname ++ " = "); println(result)
  }

  /**
   * Evaluate and nicely print function of one argument - let any
   * exceptions happen before anything printed.
   */
  def evalP1[A,B](arg: => A, f: A => B, fname: String): Unit = {
    val result = f(arg)
    print(fname); print("("); print(arg); print(") = ")
    println(result)
  }

  /**
   * Evaluate and nicely print function of two arguments - let any
   * exceptions happen before anything printed.
   */
  def evalP2[A,B,C](arg1: => A, arg2: => B, f: (A,B) => C,
                                            fname: String): Unit = {
    val result = f(arg1, arg2)
    print(fname); print("("); print(arg1)
    print(", "); print(arg2); print(") = ")
    println(result)
  }
      
  /** Test package */
  def main(args: Array[String]): Unit = {

    // Test mean and variance

    // Some test data:
    val foo = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10): List[Double]
    val bar = (0 to 100).map(_.toDouble)
    val baz = Nil: List[Double]

    println("Test mean:\n")
    evalP1(foo, mean, "mean")
    evalP1(bar, mean, "mean")
    evalP1(baz, mean, "mean")

    println("\nTest variance:\n")
    evalP1(foo, variance, "variance")
    evalP1(bar, variance, "variance")
    evalP1(baz, variance, "variance")

    println("\nTest variance1:\n")
    evalP1(foo, variance1, "variance1")
    evalP1(bar, variance1, "variance1")
    evalP1(baz, variance1, "variance1")

    println("\nTest variance2:\n")
    evalP1(foo, variance2, "variance2")
    evalP1(bar, variance2, "variance2")
    evalP1(baz, variance2, "variance2")

    println("\nTest variance2:\n")
    evalP1(foo, variance3, "variance3")
    evalP1(bar, variance3, "variance3")
    evalP1(baz, variance3, "variance3")

    // Test Try, lift, map2, lift2

    // First define some functions and Options
    val fun2 = (x: Int, y: Double) => (x * y).toString ++ " string"

    val fun2_failable = (x: Int, y: Double) =>
      if (x == 10) throw new Exception("fail!")
      else (x * y).toString

    val fun1 = (x: Int) => x + 1

    val fun1_failable = (x: Int) => 
      if (x == 42) throw new Exception("fail!")
      else x + 1

    val baz5 = Some(5)
    val baz8 = Some(8)
    val baz10 = Some(10)
    val bazN: Option[Int] = None

    val bar3 = Some(3.0)
    val barN: Option[Double] = None

    // First run naked 
    println("\nTest unlifted functions in try block:\n")
    try {
      evalP2(5, 3.0, fun2, "fun2")
      evalP2(5, 3.0, fun2_failable, "fun2_failable")
      evalP2(10, 3.0, fun2_failable, "fun2_failable")
    } catch {
      case e: Exception =>  println("An exception was caught, boohoohoo.\n")
    }

    // Next, test Try 
    println("Convert from exceptions to Options via Try:\n")
    evalP0(Try(fun1_failable(5)), "Try(fun1_failable(5))")
    evalP0(Try(fun1_failable(42)), "Try(fun1_failable(42))")
    evalP0(Try(fun2_failable(5, 5.0)), "Try(fun2_failable(5, 5.0))")
    evalP0(Try(fun2_failable(10, 5.0)), "Try(fun2_failable(10. 5.0))")

/*
    // Test lift
    println("\nTest lift:\n")
    val fun1Lifted = lift(fun1)
    evalP1(baz5, fun1Lifted, "fun1Lifted")
    evalP1(bazN, fun1Lifted, "fun1Lifted")

    // Test map2
    println("\nTest map2:\n")
    evalP0(map2(baz5, bar3)(fun2), "map2(Some(5), Some(3.0))(fun2)")
    evalP0(map2(bazN, bar3)(fun2), "map2(None, Some(3.0))(fun2)")
    evalP0(map2(baz5, barN)(fun2), "map2(Some(5), None)(fun2)")
    evalP0(map2(bazN, barN)(fun2), "map2(None, None)(fun2)")

    println("\nTest partially applied map2:\n")

    val fn = (m: Int, n: Int) => 
      if (m < 7) m
      else n

    // Doesn't seem to play nice with partial function application.
    // I had to explicitly annotate to get to work.
    val opt58 = map2(baz5, baz8)(_: (Int, Int) => Int)
    val opt85 = map2(baz8, baz5)(_: (Int, Int) => Int)
    val optN8 = map2(bazN, baz8)(_: (Int, Int) => Int)
    val optN5 = map2(bazN, baz5)(_: (Int, Int) => Int)
    val opt5N = map2(baz5, bazN)(_: (Int, Int) => Int)
    val opt8N = map2(baz8, bazN)(_: (Int, Int) => Int)
    val optNN = map2(bazN, bazN)(_: (Int, Int) => Int)

    evalP1(fn, opt58, "opt58")
    evalP1(fn, opt85, "opt85")
    evalP1(fn, optN8, "optN8")
    evalP1(fn, optN5, "optN5")
    evalP1(fn, opt5N, "opt5N")
    evalP1(fn, opt8N, "opt8N")
    evalP1(fn, optNN, "optNN")

    // Test map2r
    println("\nTest map2r directly:\n")
    evalP0(map2r(fn)(baz5, baz8), "map2r(fn)(baz5, baz8)")
    evalP0(map2r(fn)(baz8, baz5), "map2r(fn)(baz8, baz5)")
    evalP0(map2r(fn)(bazN, baz8), "map2r(fn)(bazN, baz8)")
    evalP0(map2r(fn)(bazN, baz5), "map2r(fn)(bazN, baz5)")
    evalP0(map2r(fn)(baz5, bazN), "map2r(fn)(baz5, bazN)")
    evalP0(map2r(fn)(baz8, bazN), "map2r(fn)(baz8, bazN)")
    evalP0(map2r(fn)(bazN, bazN), "map2r(fn)(bazN, bazN)")

    println("\nTest map2r partially applied:\n")
    val fnO = map2r(fn)(_, _)

    evalP2(baz5, baz8, fnO, "fnO")
    evalP2(baz8, baz5, fnO, "fnO")
    evalP2(bazN, baz8, fnO, "fnO")
    evalP2(bazN, baz5, fnO, "fnO")
    evalP2(baz5, bazN, fnO, "fnO")
    evalP2(baz8, bazN, fnO, "fnO")
    evalP2(bazN, bazN, fnO, "fnO")

    println("\nTest map2r again, more complicated types:\n")
    val fun2O = map2r(fun2)(_, _)

    evalP2(baz5, bar3, fun2O, "fun2O")
    evalP2(baz5, barN, fun2O, "fun2O")
    evalP2(bazN, bar3, fun2O, "fun2O")
    evalP2(bazN, barN, fun2O, "fun2O")

    // Test lift2
    println("\nTest lift2:\n")
    val fun2Lifted = lift2(fun2)

    evalP2(baz5, bar3, fun2Lifted, "fun2Lifted")
    evalP2(baz5, barN, fun2Lifted, "fun2Lifted")
    evalP2(bazN, bar3, fun2Lifted, "fun2Lifted")
    evalP2(bazN, barN, fun2Lifted, "fun2Lifted")

    // Test sequence and its variants
    println("\nTest various implementations of sequence:\n")

    // Test data
    val someNums = List(1,2,3,4,5,6,7,8,9,10) map (Some(_))
    val missSome = someNums map (_ filter (x => x < 4 || x > 6))

    // Scala is messing up type inference on these.
    // Had to use explicit anotation for it to work.
    // evalP1(someSome, sequence1, "sequence1")
    // evalP1(missSome, sequence1, "sequence1")
    evalP1(someNums, sequence1(_: List[Option[Int]]), "sequence1")
    evalP1(missSome, sequence1(_: List[Option[Int]]), "sequence1")
    evalP1(someNums, sequence2(_: List[Option[Int]]), "sequence2")
    evalP1(missSome, sequence2(_: List[Option[Int]]), "sequence2")
    evalP1(someNums, sequence3(_: List[Option[Int]]), "sequence3")
    evalP1(missSome, sequence3(_: List[Option[Int]]), "sequence3")
    evalP1(someNums, sequence4(_: List[Option[Int]]), "sequence4")
    evalP1(missSome, sequence4(_: List[Option[Int]]), "sequence4")

    // Test parseDoubles1
    println("\nTest parseDouble1 which uses sequence1:\n")

    val strDouble = List("1.0", "2.0", "3.0", "4.0")
    val strDoubt = List("1.0", "2.0", "Three", "4.0")
    val strInts = List("1", "2", "3", "4", "5", "6")
    val strIntsNotAll = List("1", "Two", "3", "4", "5", "6")

    evalP1(strDouble, parseDoubles1, "parseDoubles1")
    evalP1(strDoubt, parseDoubles1, "parseDoubles1")

    // Test methods based on traverse

    println("\nTest sequence based on transverse:\n")
    evalP1(someNums, sequence(_: List[Option[Int]]), "sequence")
    evalP1(missSome, sequence(_: List[Option[Int]]), "sequence")

    println("\nTest parseDouble based on transverse:\n")
    evalP1(strDouble, parseDoubles, "parseDoubles")
    evalP1(strDoubt, parseDoubles, "parseDoubles")
    evalP1(strInts, parseDoubles, "parseDoubles")
 
    println("\nTest parseInts based on transverse:\n")
    evalP1(strInts, parseInts, "parseInts")
    evalP1(strIntsNotAll, parseInts, "parseInts")
    evalP1(strDouble, parseInts, "parseInts")
 
*/
    println()

  }
}
