package grockScala.chap06.state

import grockScala.state._
import grockScala.state.RNG._

object RNGTest {

  val rng = LCG(42)

  def main(args: Array[String]): Unit = {

    // See if grockScala.state and object RNG namespaces in scope
    print("\nrng = "); println(rng)
    print("nonNegativeInt(rng)  = "); println(nonNegativeInt(rng))
    print("nonNegativeInt1(rng) = "); println(nonNegativeInt1(rng))

    println()
  }

}
