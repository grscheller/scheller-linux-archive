package grockScala.chap06.state

import grockScala.state._

object RNGTest {

  val rng = LCG(42)
  val oneToTen = List(1,2,3,4,5,6,7,8,9,10)

  def main(args: Array[String]): Unit = {

    // See if grockScala.state namespace in scope
    print("\nrng = "); println(rng)

    print("RNG.nonNegativeInt(rng)  = ")
    println(RNG.nonNegativeInt(rng))

    print("RNG.nonNegativeInt1(rng) = ")
    println(RNG.nonNegativeInt1(rng))

    // Imperitively generate 10 random doubles d, 0.0 <= d < 1.0,
    //   re-use rng from beginning.
    println("\nTen random doubles in [0,1):")
    var rngVar: RNG = rng
    var kk = 0
    while (kk < 10) {
      kk = kk + 1
      val pair = RNG.double(rngVar)  // Tuple unpacking awkward,
      val ranD = pair._1             // scala doesn't seem to like
      rngVar = pair._2               // to unpack into vars.
      println(ranD)
    }

    // Generate comma separated list of random 3D data
    // and write to disk.

    println()
  }

}
