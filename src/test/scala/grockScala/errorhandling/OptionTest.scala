package grockScala.test.errorhanding

import grockScala.errorhandling._

object Stats {

  // Exercise 4.2
  /** Computes the mean of a dataset of Doubles */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.size)

  /** Computes the variance of a dataset of Doubles */
  def variance(xs: Seq[Double]): Option[Double] = {
    val m_Op = mean(xs)
    if (m_Op == None)
      None
    else
      mean(xs.map((x: Double) => math.pow(x - m_Op.getOrElse(0.0), 2)))
  }
}

object OptionTest {

  import Stats._

  /** Test package */
  def main(args: Array[String]): Unit = {
    // Some test data:
    val foo = List(1, 2, 3, 4, 5): List[Double]
    val bar = (0 to 10).map(_.toDouble)
    val baz = Nil: List[Double]

    println(mean(foo))
    println(mean(bar))
    println(mean(baz))

    println(variance(foo))
    println(variance(bar))
    println(variance(baz))

  }
}
