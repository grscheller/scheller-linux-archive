package fpinscala.chap04.errorhandling

import fpinscala.errorhandling._
import fpinscala.errorhandling.Either._

object EitherStats {

  /** Computes the mean of a dataset of Doubles */
  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.size)

  // Actually clearer to me than the equivalent
  // for comprehension,
  /** Computes the variance of a dataset of Doubles */
  def variance(xs: Seq[Double]): Either[String, Double] =
    mean(xs) flatMap (m => mean(xs map (x => math.pow((x - m), 2))))

  // Translate outer chain above to a for comprehension.
  //
  //   Note: Only in principle could the second mean
  //         fail.  So, mean's return type prevents me
  //         from putting it in the yield.  I end up
  //         having to "unpack" it so that the yield
  //         can "repack" its value.
  //
  /** Computes the variance of a dataset of Doubles */
  def variance1(xs: Seq[Double]): Either[String, Double] =
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
  def variance2(xs: Seq[Double]): Either[String, Double] =
    mean(xs) flatMap (m =>
      mean(xs map (x => math.pow((x - m), 2))) map (v => v)
    )

  // Version using pattern matching
  /** Computes the variance of a dataset of Doubles */
  def variance3(xs: Seq[Double]): Either[String, Double] =
    mean(xs) match {
      case Right(m) =>
        mean(xs.map((x: Double) => math.pow((x - m), 2)))
      case a =>
        a
    }

}

object EitherParse {

  /**  Take a list of strings and return a Right(List[Double])
    *  of Doubles if all can be converted, Left[Exception] otherwise.
    */
  def parseDoubles1(ss: List[String]): Either[Exception, List[Double]] =
    sequence(ss map (s => Try(s.toDouble)))

  /**  Take a list of strings and return a Right(List[Double])
    *  of Doubles if all can be converted, Left[Exception] otherwise.
    */
  def parseDoubles(ss: List[String]): Either[Exception, List[Double]] =
    traverse(ss)(s => Try(s.toDouble))

  /**  Take a list of strings and return a Right(List[Int])
    *  of Ints if all can be converted, Left[Exception] otherwise.
    */
  def parseInts(ss: List[String]): Either[Exception, List[Int]] =
    traverse(ss)(s => Try(s.toInt))

}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

}

object EitherTest {

  import EitherStats._
  import EitherParse._

  // Define some utility functions
  /** Evaluate and nicely print expresion - let any
    * exceptions happen before anything printed.
    */
  def evalP0[A](expr: => A, fname: String): Unit = {
    val result = expr // Let any exceptions happen before anything printed.
    print(fname ++ " = "); println(result)
  }

  /** Evaluate and nicely print function of one argument - let any
    * exceptions happen before anything printed.
    */
  def evalP1[A, B](arg: => A, f: A => B, fname: String): Unit = {
    val result = f(arg)
    print(fname); print("("); print(arg); print(") = ")
    println(result)
  }

  /** Evaluate and nicely print function of two arguments - let any
    * exceptions happen before anything printed.
    */
  def evalP2[A, B, C](
      arg1: => A,
      arg2: => B,
      f: (A, B) => C,
      fname: String
  ): Unit = {
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

    // Test Try, lift, map2

    // First define some functions and Options
    val fun2 = (x: Int, y: Double) => (x * y).toString ++ " string"

    val fun2_failable = (x: Int, y: Double) =>
      if (x == 10) throw new Exception("fail!")
      else (x * y).toString

    val fun1 = (x: Int) => x + 1

    val fun1_failable = (x: Int) =>
      if (x == 42) throw new Exception("fail!")
      else x + 1

    val baz5: Either[String, Int] = Right(5)
    val baz8: Either[String, Int] = Right(8)
    val baz10: Either[String, Int] = Right(10)
    val bazN: Either[String, Int] = Left("bazN was used")

    val bar3: Either[String, Double] = Right(3.0)
    val barN: Either[String, Double] = Left("barN was used")

    // First run naked
    println("\nTest unwrapped functions in try block:\n")
    try {
      evalP2(5, 3.0, fun2, "fun2")
      evalP2(5, 3.0, fun2_failable, "fun2_failable")
      evalP2(10, 3.0, fun2_failable, "fun2_failable")
    } catch {
      case e: Exception => println("An exception was caught, boohoohoo.\n")
    }

    // Next, test Try
    println("Convert from exceptions to Eithers via Try:\n")
    evalP0(Try(fun1_failable(5)), "Try(fun1_failable(5))")
    evalP0(Try(fun1_failable(42)), "Try(fun1_failable(42))")
    evalP0(Try(fun2_failable(5, 5.0)), "Try(fun2_failable(5, 5.0))")
    evalP0(Try(fun2_failable(10, 5.0)), "Try(fun2_failable(10. 5.0))")

    // Test map2
    println("\nTest map2 directly:\n")
    evalP0(baz5.map2(bar3)(fun2), "baz5.map2(bar3)(fun2)")
    evalP0(bazN.map2(bar3)(fun2), "bazN.map2(bar3)(fun2)")
    evalP0(baz5.map2(barN)(fun2), "baz5.map2(barN)(fun2)")
    evalP0(bazN.map2(barN)(fun2), "bazN.map2(barN)(fun2)")

    println("\nTest map2 partially applied:\n")
    val fn = (m: Int, n: Int) =>
      if (m < 7) m
      else n

    // Annotate necessary.  Otherwise how would Scalac know
    // the return type?
    val either58 = baz5.map2(baz8)(_: (Int, Int) => Int)
    val either85 = baz8.map2(baz5)(_: (Int, Int) => Int)
    val eitherN8 = bazN.map2(baz8)(_: (Int, Int) => Int)
    val eitherN5 = bazN.map2(baz5)(_: (Int, Int) => Int)
    val either5N = baz5.map2(bazN)(_: (Int, Int) => Int)
    val either8N = baz8.map2(bazN)(_: (Int, Int) => Int)
    val eitherNN = bazN.map2(bazN)(_: (Int, Int) => Int)

    evalP1(fn, either58, "either58")
    evalP1(fn, either85, "either85")
    evalP1(fn, eitherN8, "eitherN8")
    evalP1(fn, eitherN5, "eitherN5")
    evalP1(fn, either5N, "either5N")
    evalP1(fn, either8N, "either8N")
    evalP1(fn, eitherNN, "eitherNN")

    println("\nTest map2 via Person objects\n")
    val ethel = Person.mkPerson("Ethel", 52)
    val fred1 = Person.mkPerson("", 62)
    val fred2 = Person.mkPerson("Fred", -5)

    evalP0(ethel, "ethel")
    evalP0(fred1, "fred1")
    evalP0(fred2, "fred2")

    // Test sequence and other methods based on traverse
    println("\nTest sequence and other methods based on traverse:\n")

    // Test data
    val rtNums: List[Either[String, Int]] =
      List(Right(1), Right(2), Right(3), Right(4), Right(5))
    val ltMiss: List[Either[String, Int]] =
      List(Right(1), Left("Two"), Right(3), Left("Four"), Right(5))

    evalP1(rtNums, sequence(_: List[Either[String, Int]]), "sequence")
    evalP1(ltMiss, sequence(_: List[Either[String, Int]]), "sequence")

    // Test parseDoubles1
    println("\nTest parseDouble1 which uses sequence:\n")

    val strDouble = List("1.0", "2.0", "3.0", "4.0")
    val strDoubt = List("1.0", "2.0", "Three", "4.0")
    val strInts = List("1", "2", "3", "4", "5", "6")
    val strIntsNotAll = List("1", "Two", "3", "4", "5", "6")

    evalP1(strDouble, parseDoubles1, "parseDoubles1")
    evalP1(strDoubt, parseDoubles1, "parseDoubles1")

    println("\nTest parseDoubles and parseInts which use traverse:\n")

    evalP1(strDouble, parseDoubles, "parseDoubles")
    evalP1(strDoubt, parseDoubles, "parseDoubles")
    evalP1(strInts, parseDoubles, "parseDoubles")

    evalP1(strInts, parseInts, "parseInts")
    evalP1(strIntsNotAll, parseInts, "parseInts")
    evalP1(strDouble, parseInts, "parseInts")

    println()

  }
}
