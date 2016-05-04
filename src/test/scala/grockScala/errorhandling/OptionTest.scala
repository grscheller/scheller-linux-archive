package grockScala.test.errorhanding

import grockScala.errorhandling._

object Stats {

  // Exercise 4.2 - Implement a variance function via flatMap

  /** Computes the mean of a dataset of Doubles */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.size)

  // Initial attempt without flatMap - needed to see something that worked
  // in order to figure out the flatMap version.
  /** Computes the variance of a dataset of Doubles */
  def variance1(xs: Seq[Double]): Option[Double] = {
    val m_Op = mean(xs)
    if (m_Op == None)
      None
    else
      mean(xs.map((x: Double) => math.pow(x - m_Op.getOrElse(0.0), 2)))
  }

  // Much nicer
  /** Computes the variance of a dataset of Doubles */
  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m => 
      mean(xs.map(x => math.pow((x - m), 2))))

}

object OptionTest {

  import Stats._

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
  def evalP1[A,B](arg: => A, f: (A) => B, fname: String): Unit = {
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

    println("\nTest variance1:\n")
    evalP1(foo, variance1, "variance1")
    evalP1(bar, variance1, "variance1")
    evalP1(baz, variance1, "variance1")

    println("\nTest variance:\n")
    evalP1(foo, variance, "variance")
    evalP1(bar, variance, "variance")
    evalP1(baz, variance, "variance")

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
    evalP0(Option.Try(fun1_failable(5)), "Option.Try(fun1_failable(5))")
    evalP0(Option.Try(fun1_failable(42)), "Option.Try(fun1_failable(42))")
    evalP0(Option.Try(fun2_failable(5, 5.0)),
                      "Option.Try(fun2_failable(5, 5.0))")
    evalP0(Option.Try(fun2_failable(10, 5.0)),
                       "Option.Try(fun2_failable(10. 5.0))")

    // Test lift
    println("\nTest lift:\n")
    val fun1Lifted = Option.lift(fun1)
    evalP1(baz5, fun1Lifted, "fun1Lifted")
    evalP1(bazN, fun1Lifted, "fun1Lifted")

    // Test map2
    println("\nTest map2:\n")
    evalP0(Option.map2(baz5, bar3)(fun2),
                       "Option.map2(Some(5), Some(3.0))(fun2)")
    evalP0(Option.map2(bazN, bar3)(fun2),
                       "Option.map2(None, Some(3.0))(fun2)")
    evalP0(Option.map2(baz5, barN)(fun2),
                       "Option.map2(Some(5), None)(fun2)")
    evalP0(Option.map2(bazN, barN)(fun2),
                       "Option.map2(None, None)(fun2)")

    println("\nTest partially applied map2:\n")

    val fn = (m: Int, n: Int) => 
      if (m < 7) m
      else n

    // Doesn't seem to play nice with partial function application.
    // I had to explicitly annotate to get to work.
    val opt58 = Option.map2(baz5, baz8)(_: (Int, Int) => Int)
    val opt85 = Option.map2(baz8, baz5)(_: (Int, Int) => Int)
    val optN8 = Option.map2(bazN, baz8)(_: (Int, Int) => Int)
    val optN5 = Option.map2(bazN, baz5)(_: (Int, Int) => Int)
    val opt5N = Option.map2(baz5, bazN)(_: (Int, Int) => Int)
    val opt8N = Option.map2(baz8, bazN)(_: (Int, Int) => Int)
    val optNN = Option.map2(bazN, bazN)(_: (Int, Int) => Int)

    evalP1(fn, opt58, "opt58")
    evalP1(fn, opt85, "opt85")
    evalP1(fn, optN8, "optN8")
    evalP1(fn, optN5, "optN5")
    evalP1(fn, opt5N, "opt5N")
    evalP1(fn, opt8N, "opt8N")
    evalP1(fn, optNN, "optNN")

    // Test map2r
    println("\nTest map2r directly:\n")
    evalP0(Option.map2r(fn)(baz5, baz8), "Option.map2r(fn)(baz5, baz8)")
    evalP0(Option.map2r(fn)(baz8, baz5), "Option.map2r(fn)(baz8, baz5)")
    evalP0(Option.map2r(fn)(bazN, baz8), "Option.map2r(fn)(bazN, baz8)")
    evalP0(Option.map2r(fn)(bazN, baz5), "Option.map2r(fn)(bazN, baz5)")
    evalP0(Option.map2r(fn)(baz5, bazN), "Option.map2r(fn)(baz5, bazN)")
    evalP0(Option.map2r(fn)(baz8, bazN), "Option.map2r(fn)(baz8, bazN)")
    evalP0(Option.map2r(fn)(bazN, bazN), "Option.map2r(fn)(bazN, bazN)")

    println("\nTest map2r partially applied:\n")
    val fnO = Option.lift2(fn)(_, _)

    evalP2(baz5, baz8, fnO, "fnO")
    evalP2(baz8, baz5, fnO, "fnO")
    evalP2(bazN, baz8, fnO, "fnO")
    evalP2(bazN, baz5, fnO, "fnO")
    evalP2(baz5, bazN, fnO, "fnO")
    evalP2(baz8, bazN, fnO, "fnO")
    evalP2(bazN, bazN, fnO, "fnO")

    println("\nTest map2r again, more complicated types:\n")
    val fun2O = Option.lift2(fun2)(_, _)

    evalP2(baz5, bar3, fun2O, "fun2O")
    evalP2(baz5, barN, fun2O, "fun2O")
    evalP2(bazN, bar3, fun2O, "fun2O")
    evalP2(bazN, barN, fun2O, "fun2O")

    // Test lift2
    println("\nTest lift2:\n")
    val fun2Lifted = Option.lift2(fun2)

    evalP2(baz5, bar3, fun2Lifted, "fun2Lifted")
    evalP2(baz5, barN, fun2Lifted, "fun2Lifted")
    evalP2(bazN, bar3, fun2Lifted, "fun2Lifted")
    evalP2(bazN, barN, fun2Lifted, "fun2Lifted")

    // Test sequence and its variants
    println("\nTest various implementations of sequence:\n")

    // Test data (thrashing with Scala collections)
    // val some2to30: List[Option[Int]] = (2 to 30 by 2).to[List] map (Some(_))
    // val missing2to30 = some2to30 map (_ filter (x => x > 20 || x < 10))
    val some1to10 = List(1,2,3,4,5,6,7,8,9,10) map (Some(_))
    val miss1to10 = some1to10 map (_ filter (x => x < 4 || x > 6))

    println(Option.sequence1(some1to10))
    println(Option.sequence1(miss1to10))
    println()
    println(Option.sequence2(some1to10))
    println(Option.sequence2(miss1to10))

    // evalP1(some1to10, Option.sequence1, "Option.sequence1")
    // evalP1(miss1to10, Option.sequence1, "Option.sequence1")
    // evalP1(some1to10, Option.sequence2, "Option.sequence2")
    // evalP1(miss1to10, Option.sequence2, "Option.sequence2")
    // evalP1(some1to10, Option.sequence, "Option.sequence")
    // evalP1(miss1to10, Option.sequence, "Option.sequence")
    
    println()

  }
}
