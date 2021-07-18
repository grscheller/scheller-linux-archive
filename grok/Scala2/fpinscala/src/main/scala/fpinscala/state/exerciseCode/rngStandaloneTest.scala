package fpinscala.chap06.rngStandalone

import fpinscala.rngStandalone.{RNG, LCG}
import fpinscala.rngStandalone.RNG.Rand

object rngStandaloneTest {

  val rng42 = LCG(42)
  val rng666 = LCG(666)

  def main(args: Array[String]): Unit = {

    // See if fpinscala.state namespace in scope
    print("\nrng42 = "); println(rng42)

    print("RNG.nonNegativeInt(rng42)  = ")
    println(RNG.nonNegativeInt(rng42))

    print("RNG.nonNegativeInt1(rng42) = ")
    println(RNG.nonNegativeInt1(rng42))

    // Imperatively generate 10 random doubles d, 0.0 <= d < 1.0,
    println("\nImperitively print ten random doubles in [0,1):")

    var rngVar: RNG = rng42
    var kk = 0
    while (kk < 10) {
      kk = kk + 1
      val pair = RNG.double(rngVar) // Tuple unpacking awkward,
      val ranD = pair._1            // scala doesn't seem to like
      rngVar = pair._2              // to unpack into existing vars.
      println(ranD)
    }

    // Repeat but more functionally using std scala libraries
    println("\nFunctionally print ten random doubles in [0,1):")

    val getNextRanPair = (x: (Double, RNG)) => RNG.double(x._2)
    val rngS =
      LazyList.iterate(RNG.double(rng42))(getNextRanPair) map (_._1) take 10
    for (ranDouble <- rngS) println(ranDouble)

    // Test intDouble and doubleInt
    println("\nThese next two should be reverse of each other:")
    print("RNG.intDouble(rng42) = "); println(RNG.intDouble(rng42))
    print("RNG.doubleInt(rng42) = "); println(RNG.doubleInt(rng42))

    // Test double3
    val (firstTuple, rngFT) = RNG.double3(rng42)
    val (secondTuple, rngST) = RNG.double3(rngFT)
    val (thirdTuple, _) = RNG.double3(rngST)
    println("\nPrint 3 double 3-tuples:")
    println(firstTuple); println(secondTuple); println(thirdTuple)

    // Test ints1
    println("\nTest ints1:")
    val (twoList1, rng11) = RNG.ints1(2)(rng42)
    val (emptyList1, rng12) = RNG.ints1(0)(rng11)
    val (sixList1, _) = RNG.ints1(6)(rng12)
    print("\ntwoList1 = "); println(twoList1)
    print("emptyList1 = "); println(emptyList1)
    print("sixList1 = "); println(sixList1)

    // Test ints
    println("\nTest ints:")
    val (twoList, rng1) = RNG.ints(2)(rng42)
    val (emptyList, rng2) = RNG.ints(0)(rng1)
    val (sixList, _) = RNG.ints(6)(rng2)
    print("\ntwoList = "); println(twoList)
    print("emptyList = "); println(emptyList)
    print("sixList = "); println(sixList)

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

    // Auto-roll some dice - test 5 sequence implementations
    println("\nAuto roll 2 dice 10 times:")

    val diceRolls10 = List.fill(10)(twoDiceRoll)

    val rollSomeRecur10 = RNG.sequenceRecursion(diceRolls10)
    val rollSomeFR10 = RNG.sequenceFR(diceRolls10)
    val rollSomeFLRev10 = RNG.sequenceFLRev(diceRolls10)
    val rollSomeFL10 = RNG.sequenceFL(diceRolls10)
    val rollSomeRevFL10 = RNG.sequenceRevFL(diceRolls10)

    val (rolledDiceRecur10, _) = rollSomeRecur10(rngD)
    val (rolledDiceFR10, _) = rollSomeFR10(rngD)
    val (rolledDiceFLRev10, _) = rollSomeFLRev10(rngD)
    val (rolledDiceFL10, _) = rollSomeFL10(rngD)
    val (rolledDiceRevFL10, _) = rollSomeRevFL10(rngD)

    print("rolledSomeRecur10 = "); println(rolledDiceRecur10)
    print("rolledSomeFR10 = "); println(rolledDiceFR10)
    print("rolledSomeFLRev10 = "); println(rolledDiceFLRev10)
    print("rolledSomeFL10 = "); println(rolledDiceFL10)
    print("rolledSomeRevFL10 = "); println(rolledDiceRevFL10)

    // Test how stack-safe RNG.sequence implementations are
    println("\nAuto roll 2 dice large number of times and find average:")

    var numRolls = 2500
    var diceRolls = List.fill(numRolls)(twoDiceRoll)
    val (rolledDiceRecur, _) = RNG.sequenceRecursion(diceRolls)(rng42)
    print("Average of " + numRolls + " two dice rolls(Recur) is ")
    println(rolledDiceRecur.sum.toDouble / numRolls)

    numRolls = 2500
    diceRolls = List.fill(numRolls)(twoDiceRoll)
    val (rolledDiceFR, _) = RNG.sequenceFR(diceRolls)(rng42)
    print("Average of " + numRolls + " two dice rolls(FR)    is ")
    println(rolledDiceFR.sum.toDouble / numRolls)

    numRolls = 2500
    diceRolls = List.fill(numRolls)(twoDiceRoll)
    val (rolledDiceFLRev, _) = RNG.sequenceFLRev(diceRolls)(rng42)
    print("Average of " + numRolls + " two dice rolls(FLRev) is ")
    println(rolledDiceFLRev.sum.toDouble / numRolls)

    numRolls = 2500
    diceRolls = List.fill(numRolls)(twoDiceRoll)
    val (rolledDiceFL, _) = RNG.sequenceFL(diceRolls)(rng42)
    print("Average of " + numRolls + " two dice rolls(FL)    is ")
    println(rolledDiceFL.sum.toDouble / numRolls)

    numRolls = 3700
    diceRolls = List.fill(numRolls)(twoDiceRoll)
    val (rolledDiceRevFL, _) = RNG.sequenceRevFL(diceRolls)(rng42)
    print("Average of " + numRolls + " two dice rolls(RevFL) is ")
    println(rolledDiceRevFL.sum.toDouble / numRolls)

    // I need to straighten this out, the difference
    // between RNG.sequenceFL and RNG.sequenceFLRev.
    // What is confusimg me is the above list are all
    // the "same" random action (twoDiceRoll: Rand).
    println("\nCreate a random non-homogenous random sequence -")
    val powOfTen: List[Double] = List(1, 10, 100, 1000, 10000)
    val scaledRandomDoubles: List[Rand[Double]] =
      powOfTen.map(ii => RNG.map(RNG.double)(_ * ii))

    val scaledRandomSequenceRecur = RNG.sequenceFR(scaledRandomDoubles)
    val scaledRandomSequenceFR = RNG.sequenceFR(scaledRandomDoubles)
    val scaledRandomSequenceFLRev = RNG.sequenceFLRev(scaledRandomDoubles)
    val scaledRandomSequenceFL = RNG.sequenceFL(scaledRandomDoubles)
    val scaledRandomSequenceRevFL = RNG.sequenceRevFL(scaledRandomDoubles)

    val (resultRecur, _) = scaledRandomSequenceRecur(rng666)
    val (resultFR, _) = scaledRandomSequenceFR(rng666)
    val (resultFLRev, _) = scaledRandomSequenceFLRev(rng666)
    val (resultFL, _) = scaledRandomSequenceFL(rng666)
    val (resultRevFL, _) = scaledRandomSequenceRevFL(rng666)

    print("  with recursion:\n    ")
    println(resultRecur)

    print("  fold right:\n    ")
    println(resultFR)

    print("  fold left then reverse:\n    ")
    println(resultFLRev)

    print("  fold left:\n    ")
    println(resultFL)

    print("  reverse then fold left:\n    ")
    println(resultRevFL)

    // Look at ints again.
    println("\nLook at ints again:\n")

    print("Compare with different RNGs:")
    val (tenInts1, _) = RNG.ints(10)(rng42)
    val (tenInts2, _) = RNG.ints(10)(rng666)
    print("\n10 Ints: "); tenInts1 foreach { x => print(s"$x ") }
    print("\n10 Ints: "); tenInts2.foreach { x => print(s"$x ") }

    print("\n\nTest if ints can be partially applied:")
    val tenInts = RNG.ints(10)(_)
    val tenInts3 = tenInts(rng666)._1
    print("\n10 Ints: ")
    tenInts3 foreach { x => print(s"$x ") }
    print("\n10 Ints: ")
    tenInts(rng666)._1 foreach { x => print(s"$x ") }

    print("\n\nCompare different syntax:")
    print("\n10 Ints: ")
    RNG.ints(10)(rng666)._1.foreach(x => print(s"$x "))
    print("\n10 Ints: ")
    RNG.ints(10)(rng666)._1 foreach { x => print(s"$x ") }

    // Test nonNegativeLessThan implementations

    println("\n\nTest nonNegativeLessThan implementations")
    def foo(num: Int, lt: Int): Rand[List[Int]] =
      RNG.sequence(List.fill(num)(RNG.nonNegativeLessThanManual(lt)))

    def bar(num: Int, lt: Int): Rand[List[Int]] =
      RNG.sequence(List.fill(num)(RNG.nonNegativeLessThanNonUniform(lt)))

    def baz(num: Int, lt: Int): Rand[List[Int]] =
      RNG.sequence(List.fill(num)(RNG.nonNegativeLessThan(lt)))

    print("\n100 random non-neg Ints less than 10")
    println(" (using nonNegativeLessThanManual):")
    for (ii <- foo(100, 10)(rng42)._1) { print(ii); print(" ") }

    print("\n100 random non-neg Ints less than 10")
    println(" (using nonNegativeLessThanNonUniform):")
    for (ii <- bar(100, 10)(rng42)._1) { print(ii); print(" ") }

    print("\n100 random non-neg Ints less than 10")
    println(" (using nonNegativeLessThan):")
    for (ii <- baz(100, 10)(rng42)._1) { print(ii); print(" ") }

    print("\n\nCompare nonNegativeLessThanNonUniform ")
    println("vs nonNegativeLessThanManual:")
    val midRoad = 200000000
    for (
      intPair <- foo(100, midRoad)(LCG(3))._1.zip(bar(100, midRoad)(LCG(3))._1)
    ) {
      println(intPair)
    }

    print("\n\nCompare nonNegativeLessThanManual ")
    println("vs nonNegativeLessThan:")
    for (
      intPair <- foo(15, midRoad)(rng42)._1.zip(baz(15, midRoad)(rng42)._1)
    ) {
      println(intPair)
    }

    print("\n\nCompare nonNegativeLessThanManual ")
    println("vs nonNegativeLessThan as a single random action:")
    val foobaz = RNG.both(foo(10, midRoad), baz(10, midRoad))(rng42)._1
    for (intPair <- foobaz._1.zip(foobaz._2)) println(intPair)

    // Test map_ and map2_
    println("\nTest map_ and map2_ by throwing dice:\n")

    def dieRollFM: RNG.Rand[Int] =
      RNG.map_(RNG.nonNegativeLessThan(6))(_ + 1)

    def twoDiceRollFM: RNG.Rand[Int] =
      RNG.map2_(dieRollFM, dieRollFM)(_ + _)

    def throwSome(num: Int): Rand[List[Int]] =
      RNG.sequence(List.fill(num)(twoDiceRollFM))

    print("throwSome(20)(rng42)._1 = ")
    println(throwSome(20)(rng42)._1)

    //  Lets try and do a for comprehension:

//  The following naive use of a "for comprehension"
//  will not work!
//
//  def makeRandList(lt: Int): Rand[List[Int]] = for {
//    n   <- RNG.nonNegativeLessThan(lt)
//    d   <- RNG.nonNegativeLessThan(n)
//    ns  <- RNG.unit(RNG.ints(n)(_))
//  } yield ns._1 map { _.abs % (d + 1) }
//
//  I get error messages like:
//
//    value flatMap is not a member of
//    fpinscala.rngStandalone.RNG.Rand[Int]
//
//  which is just a type alias for function type RNG => (Int, RNG).
//
//  The problem is that flatMap and map are defined in
//  the RNG companion object.  This is a strong indication
//  that we need to encapsulate the Rand[A] type into some
//  sort of class or trait that can have flatMap and map
//  methods.
//
//  The Function1 trait doesn't have flatMap and map methods,
//  hence the errors we get.
//

    // Try to implement above with RNG companion
    // object methods directly.

    println("\nMake some length/value bounded List[Int]:\n")

    /** Return a random action that will
      *  product a List of length less than lt
      *  and whose values are bounded by the
      *  List's length.
      */
    def makeRandList(lt: Int): Rand[List[Int]] =
      RNG.flatMap(RNG.nonNegativeLessThan(lt)) { n =>
        RNG.flatMap(RNG.nonNegativeLessThan(n)) { d =>
          RNG.map(RNG.ints(n)) { ns =>
            ns map { _.abs % (d + 1) }
          }
        }
      }

    val myRandList10 = makeRandList(10)
    val myRandList20 = makeRandList(20)

    print("myRandList10(rng42)._1 = ")
    println(myRandList10(rng42)._1)

    print("myRandList10(rng666)._1 = ")
    println(myRandList10(rng666)._1)

    print("myRandList20(rng42)._1 = ")
    println(myRandList20(rng42)._1)

    print("myRandList20(rng666)._1 = ")
    println(myRandList20(rng666)._1)

    println()

  }
}
