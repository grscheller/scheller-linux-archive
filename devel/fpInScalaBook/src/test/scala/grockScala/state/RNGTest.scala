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
    println("\nImperitively print ten random doubles in [0,1):")

    var rngVar: RNG = rng
    var kk = 0
    while (kk < 10) {
      kk = kk + 1
      val pair = RNG.double(rngVar)  // Tuple unpacking awkward,
      val ranD = pair._1             // scala doesn't seem to like
      rngVar = pair._2               // to unpack into existing vars.
      println(ranD)
    }

    // Repeat but more functionally using std scala libraries
    println("\nFunctionally print ten random doubles in [0,1):")

    val getNextRanPair = (x: (Double, RNG)) => RNG.double(x._2)
    val rngS = Stream.iterate(RNG.double(rng))(getNextRanPair) map (_._1) take 10
    for (ranDouble <- rngS) println(ranDouble)

    // Generate comma separated list of random 3D data
    // and write to disk.

    println()
  }

}
