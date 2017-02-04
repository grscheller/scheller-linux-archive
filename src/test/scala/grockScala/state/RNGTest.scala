package grockScala.test.chap06.state

import grockScala.state.{RNG, LCG}
import grockScala.state.RNG.Rand

object RNGTest {

  val rng42 = LCG(42)
  val rng666 = LCG(666)
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
    val rollSome  = RNG.sequenceRecursion(diceRolls)
    val (rolledDice, _) = rollSome(rngD)
    print("rolledSome  = "); println(rolledDice)

    // Auto-roll some dice - test 3 other implementations
    val rollSomeRecursion = RNG.sequenceRecursion(diceRolls)
    val rollSomeFLRev = RNG.sequenceFLRev(diceRolls)
    val rollSomeFL = RNG.sequenceFL(diceRolls)
    val (rolledDiceRecursion, _) = rollSomeRecursion(rngD)
    val (rolledDiceFLRev, _) = rollSomeFLRev(rngD)
    val (rolledDiceFL, _) = rollSomeFL(rngD)
    print("rolledSomeRecursion = "); println(rolledDiceRecursion)
    print("rolledSomeFLRev = "); println(rolledDiceFLRev)
    print("rolledSomeFL = "); println(rolledDiceFL)

    // Test how stack-safe RNG.sequence implementations are
    println("\nAuto roll 2 dice large number of times and find average:")
    var numRolls = 2500
    val diceRollsRecursion1 = List.fill(numRolls)(twoDiceRoll)
    val rollSomeRecursion1  = RNG.sequenceRecursion(diceRollsRecursion1)
    val (rolledDiceRecursion1, _) = rollSomeRecursion1(rng42)
    print("Average of " + numRolls + " two dice rolls(Recur) = ")
    println(rolledDiceRecursion1.sum.toDouble/numRolls)

    // Test the other implementations of sequence for stack safety
    numRolls = 3000
    val diceRollsFoldRight1 = List.fill(numRolls)(twoDiceRoll)
    val rollSomeFoldRight1 = RNG.sequenceFR(diceRollsFoldRight1)
    val (rolledDiceFoldRight1, _) = rollSomeFoldRight1(rng42)
    print("Average of " + numRolls + " two dice rolls(FR)    = ")
    println(rolledDiceFoldRight1.sum.toDouble/numRolls)

    numRolls = 3500
    val diceRollsFoldLeft1 = List.fill(numRolls)(twoDiceRoll)
    val rollSomeFoldLeft1 = RNG.sequenceFL(diceRollsFoldLeft1)
    val (rolledDiceFoldLeft1, _) = rollSomeFoldLeft1(rng42)
    print("Average of " + numRolls + " two dice rolls(FL)    = ")
    println(rolledDiceFoldLeft1.sum.toDouble/numRolls)

    numRolls = 3500
    val diceRollsFoldLeftRev1 = List.fill(numRolls)(twoDiceRoll)
    val rollSomeFoldLeftRev1 = RNG.sequenceFLRev(diceRollsFoldLeftRev1)
    val (rolledDiceFoldLeftRev1, _) = rollSomeFoldLeftRev1(rng42)
    print("Average of " + numRolls + " two dice rolls(FLRev) = ")
    println(rolledDiceFoldLeftRev1.sum.toDouble/numRolls)

    // I need to straighten out this the difference
    // between RNG.sequenceFL and RNG.sequenceFLRev.
    // What is confusimg me is the above list are all
    // the "same" random action (twoDiceRoll: Rand).
    println("\nCreate a random non-homogenous random sequence:")
    val powOfTen: List[Double] = List(1,10,100,1000,10000)
    val scaledRandomDoubles: List[Rand[Double]] =
      powOfTen.map(ii => RNG.map(RNG.double)(_ * ii))

    val scaledRandomSequenceRecur = RNG.sequenceFR(scaledRandomDoubles)
    val scaledRandomSequenceFR    = RNG.sequenceFR(scaledRandomDoubles)
    val scaledRandomSequenceFL    = RNG.sequenceFL(scaledRandomDoubles)
    val scaledRandomSequenceFLRev = RNG.sequenceFLRev(scaledRandomDoubles)

    val (resultRecur, _) = scaledRandomSequenceRecur(rng666)
    val (resultFR, _) = scaledRandomSequenceFR(rng666)
    val (resultFL, _) = scaledRandomSequenceFL(rng666)
    val (resultFLRev, _) = scaledRandomSequenceFLRev(rng666)

    print("With Recursion:\n   ")
    println(resultRecur)

    print("With Fold Right:\n   ")
    println(resultFR)

    print("With Fold Left:\n   ")
    println(resultFL)

    print("With Reversed Fold Left:\n   ")
    println(resultFLRev)

    // Look at ints again.
    val (tenInts1, _) = RNG.ints(10)(rng42)
    val (tenInts2, _) = RNG.ints(10)(rng666)
    print("\n10 Ints: "); tenInts1.foreach(x => print(x + " "))
    print("\n10 Ints: "); tenInts2.foreach(x => print(x + " "))

    println()
  }

}
