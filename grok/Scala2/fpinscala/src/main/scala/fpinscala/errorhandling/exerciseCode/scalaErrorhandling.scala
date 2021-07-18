package fpinscala.chap04.errorhandling

import scala.util.{Try, Success, Failure}

object Parsing {

  /**
   *  Traverse a list and apply a function that can fail to each element.
   *
   *  Take a list, apply a function which returns an Option
   *  to each element of the list and if none are None,
   *  return an Option of a list of all the values in the
   *  Somes, otherwise return a None.
   *
   *  @param as List of type A.
   *  @param f A function of type A => Option[B].
   *  @return An Option[List[B]].
   *
   */
  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])(
      (a, bsO) =>
        for {
          bs <- bsO
          b <- f(a)
        } yield b :: bs
    )

  /**
   *  Parse a list of Doubles.
   *
   *  Take a list of strings and return an Option of a List
   *  of Doubles if all can be converted.
   */
  def parseDoubles(ss: List[String]): Option[List[Double]] =
    traverse(ss)(s => Try(s.toDouble).toOption)

}

object scalaErrorhandling{

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

  /** Computes the mean of a dataset of Doubles */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.size)

  /** Test package */
  def main(args: Array[String]): Unit = {

    import Parsing._

    // Test flatten and flatmap

    println("\nTest flatten:\n")
    val mule = Some(42)
    val goat = Some(Some(42))
    val frog: Option[Option[Double]] = None
    evalP0(mule, "mule")
    // evalP0(mule.flatten, "mule.flatten")     /* Does not compile. */
    evalP0(goat, "goat")
    evalP0(goat.flatten, "goat.flatten")
    evalP0(frog, "frog")
    evalP0(frog.flatten, "frog.flatten")

    println("\nTest traverse via parseDoubles:\n")
    val goodDoubleStrings = List("1.2", "3.14159", "10.3", "6", "7.1")
    val goodDoubles = parseDoubles(goodDoubleStrings)
    val goodMean = goodDoubles flatMap mean

    val failDoubleStrings = List("1.2", "3.14159", "Fred", "6", "8.9")
    val failDoubles = parseDoubles(failDoubleStrings)
    val failMean = failDoubles flatMap mean

    val noDoubleStrings: List[String] = List()
    val noDoubles = parseDoubles(noDoubleStrings)
    val noMean = noDoubles flatMap mean

    evalP0(goodDoubleStrings, "goodDoubleStrings")
    evalP0(goodDoubles, "goodDoubles")
    evalP0(goodMean, "goodMean")

    evalP0(failDoubleStrings, "failDoubleStrings")
    evalP0(failDoubles, "failDoubles")
    evalP0(failMean, "failMean")

    evalP0(noDoubleStrings, "noDoubleStrings")
    evalP0(noDoubles, "noDoubles")
    evalP0(noMean, "noMean")

    println()

  }
}
