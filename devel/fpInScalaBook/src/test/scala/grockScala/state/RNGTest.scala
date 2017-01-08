package grockScala.chap06.state

import grockScala.state._

object RNGTest {

  val rng42 = LCG(42)
  val oneToTen = List(1,2,3,4,5,6,7,8,9,10)

  def main(args: Array[String]): Unit = {

    // See if grockScala.state namespace in scope
    print("\nrng42 = "); println(rng42)

    print("RNG.nonNegativeInt(rng42)  = ")
    println(RNG.nonNegativeInt(rng42))

    print("RNG.nonNegativeInt1(rng42) = ")
    println(RNG.nonNegativeInt1(rng42))

    // Imperitively generate 10 random doubles d, 0.0 <= d < 1.0,
    println("\nImperitively print ten random doubles in [0,1):")

    var rngVar: RNG = rng42
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
    val rngS = Stream.iterate(RNG.double(rng42))(getNextRanPair) map (_._1) take 10
    for (ranDouble <- rngS) println(ranDouble)

    // Test intDouble and doubleInt
    println("\nTheses next two should be reverse of each other:")
    print("RNG.intDouble(rng42) = "); println(RNG.intDouble(rng42))
    print("RNG.doubleInt(rng42) = "); println(RNG.doubleInt(rng42))

    // Test double3
    val ( firstTuple, rngFT) = RNG.double3(rng42)
    val (secondTuple, rngST) = RNG.double3(rngFT)
    val ( thirdTuple,   _  ) = RNG.double3(rngST)
    println("\nPrint 3 double 3-tuples:")
    println(firstTuple); println(secondTuple); println(thirdTuple)

    // Test ints
    println("\nTest ints:")
    val (  twoList, rng1) = RNG.ints(2)(rng42)
    val (emptyList, rng2) = RNG.ints(0)(rng1)
    val (  sixList,  _  ) = RNG.ints(6)(rng2)
    print("\nPrint twoList = "); println(twoList)
    print("Print emptyList = "); println(emptyList)
    print("Print sixList = ");   println(sixList)

    // Test nonNegativeEven
    println("\nTest nonNegativeEven:")
    val (evenA, rngA) = RNG.nonNegativeEven(rng42)
    val (evenB, rngB) = RNG.nonNegativeEven(rngA)
    val (evenC, rngC) = RNG.nonNegativeEven(rngB)
    val (evenD, rngD) = RNG.nonNegativeEven(rngC)
    print("\nPrint evenA = "); println(evenA)
    print("Print evenB = "); println(evenB)
    print("Print evenC = "); println(evenC)
    print("Print evenD = "); println(evenD)

    // Generate comma separated list of random 3D data
    // and write to disk.  (do in its own test)

    println()
  }

}
