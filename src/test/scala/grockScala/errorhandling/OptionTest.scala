package grockScala.test.errorhanding

import grockScala.errorhandling._

object Stats {

  // Exercise 4.2 - Implement a variance function via flatMap

  /** Computes the mean of a dataset of Doubles */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.size)

  // Initial attempt without flatMap - needed to something working
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

  /** Test package */
  def main(args: Array[String]): Unit = {
    // Some test data:
    val foo = List(1, 2, 3, 4, 5): List[Double]
    val bar = (0 to 10000).map(_.toDouble)
    val baz = Nil: List[Double]

    println("Test mean:")
    println(mean(foo))
    println(mean(bar))
    println(mean(baz))

    println("\nTest variance1:")
    println(variance1(foo))
    println(variance1(bar))
    println(variance1(baz))

    println("\nTest variance:")
    println(variance(foo))
    println(variance(bar))
    println(variance(baz))

  }
}
