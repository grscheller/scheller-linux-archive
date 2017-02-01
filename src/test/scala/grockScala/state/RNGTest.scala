package grockScala.test.chap06.state

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
    println("\nThese next two should be reverse of each other:")
    print("RNG.intDouble(rng42) = "); println(RNG.intDouble(rng42))
    print("RNG.doubleInt(rng42) = "); println(RNG.doubleInt(rng42))

    // Test double3
    val ( firstTuple, rngFT) = RNG.double3(rng42)
    val (secondTuple, rngST) = RNG.double3(rngFT)
    val ( thirdTuple,   _  ) = RNG.double3(rngST)
    println("\nPrint 3 double 3-tuples:")
    println(firstTuple); println(secondTuple); println(thirdTuple)

    // Test ints1
    println("\nTest ints1:")
    val (  twoList1, rng11) = RNG.ints1(2)(rng42)
    val (emptyList1, rng12) = RNG.ints1(0)(rng11)
    val (  sixList1,  _  ) = RNG.ints1(6)(rng12)
    print("\ntwoList1 = "); println(twoList1)
    print("emptyList1 = "); println(emptyList1)
    print("sixList1 = ");   println(sixList1)

    // Test ints
    println("\nTest ints:")
    val (  twoList, rng1) = RNG.ints(2)(rng42)
    val (emptyList, rng2) = RNG.ints(0)(rng1)
    val (  sixList,  _  ) = RNG.ints(6)(rng2)
    print("\ntwoList = "); println(twoList)
    print("emptyList = "); println(emptyList)
    print("sixList = ");   println(sixList)

    // Test nonNegativeEven
    println("\nTest nonNegativeEven:")
    val (evenA, rngA) = RNG.nonNegativeEven(rng42)
    val (evenB, rngB) = RNG.nonNegativeEven(rngA)
    val (evenC, rngC) = RNG.nonNegativeEven(rngB)
    val (evenD, rngD) = RNG.nonNegativeEven(rngC)
    print("\nevenA = "); println(evenA)
    print("evenB = "); println(evenB)
    print("evenC = "); println(evenC)
    print("evenD = "); println(evenD)

    // Test map and map2
    println("\nTest map and map2 by throwing dice:")

    def dieRoll: RNG.Rand[Int] =
      RNG.map(RNG.nonNegativeInt)(die => die % 6 + 1)

    def twoDiceRoll: RNG.Rand[Int] =
      RNG.map2(dieRoll, dieRoll)(_ + _)

    // Some manuel rolls to start off
    println("\nManually roll 2 dice 4 times:")
    val (twoDiceRoll1, rngR1) = twoDiceRoll(rngD)
    val (twoDiceRoll2, rngR2) = twoDiceRoll(rngR1)
    val (twoDiceRoll3, rngR3) = twoDiceRoll(rngR2)
    val (twoDiceRoll4, rngR4) = twoDiceRoll(rngR3)
    print("twoDiceRoll1 = "); println(twoDiceRoll1)
    print("twoDiceRoll2 = "); println(twoDiceRoll2)
    print("twoDiceRoll3 = "); println(twoDiceRoll3)
    print("twoDiceRoll4 = "); println(twoDiceRoll4)

    // Auto-roll some dice
    println("\nAuto roll 2 dice 10 times:")
    val diceRolls = List.fill(10)(twoDiceRoll)
    val rollSome = RNG.sequence(diceRolls)
    val (rolledDice, _) = rollSome(rngD)
    print("Auto-rolled = \n   "); println(rolledDice)

    // Generate comma separated list of random 2D data
    // and write to disk.  (do in its own test)

    println()
  }

}
