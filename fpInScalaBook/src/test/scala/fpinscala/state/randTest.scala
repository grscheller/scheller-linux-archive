/** Package to test the fpinscala.rand package and 
 *  indirectly fpinscala.state.
 *
 *  Deliberately not importing fpinscala.state
 *  since I feel it is an implementation detail
 *  of the fpinscala.rng package.
 *
 *  At least initialy, repeat tests done in
 *  the fpinscala.test.chap06.rngStandalone
 *  package.
 *
 */
package fpinscala.test.chap06.rand

import fpinscala.state.rand.{Rand,RNG,LCG}

object RandTest {

  val rng42 = LCG(42)
  val rng666 = LCG(666)

  def main(args: Array[String]): Unit = {

    // See if fpinscala.state namespace in scope
    print("\nrng42 = "); println(rng42)

    print("RNG.nonNegativeInt.extract(rng42)  = ")
    println(RNG.nonNegativeInt.extract(rng42))

    // Imperitively generate 10 random doubles d, 0.0 <= d < 1.0,
    println("\nImperitively print ten random doubles in [0,1):")

    var rngVar: RNG = rng42
    var kk = 0
    while (kk < 10) {
      kk = kk + 1
      val pair = RNG.double.run(rngVar)
      val ranD = pair._1
      rngVar = pair._2
      println(ranD)
    }

    // Repeat again manually but done more functionally.
    println("\nUse a Stream to print ten random doubles in [0,1):")

    val getNextRanPair = (x: (Double, RNG)) => RNG.double.run(x._2)
    val rngS = Stream.iterate(RNG.double.run(rng42))(getNextRanPair) map (_._1) take 10
    for (ranDouble <- rngS) println(ranDouble)

    // Test ints
    println("\nTest ints:")
    val (  twoList, rng1) = RNG.ints(2).run(rng42)
    val (emptyList, rng2) = RNG.ints(0).run(rng1)
    val (  sixList,  _  ) = RNG.ints(6).run(rng2)
    print("\ntwoList = "); println(twoList)
    print("emptyList = "); println(emptyList)
    print("sixList = ");   println(sixList)

    // Test nonNegativeEven
    println("\nTest nonNegativeEven:")
    val (evenA, rngA) = RNG.nonNegativeEven.run(rng42)
    val (evenB, rngB) = RNG.nonNegativeEven.run(rngA)
    val (evenC, rngC) = RNG.nonNegativeEven.run(rngB)
    val (evenD, rngD) = RNG.nonNegativeEven.run(rngC)
    print("\nevenA = "); println(evenA)
    print("evenB = "); println(evenB)
    print("evenC = "); println(evenC)
    print("evenD = "); println(evenD)

    // Test map and map2
    println("\nTest map and map2 by throwing dice:")

    def dieRoll: Rand[Int] =
      RNG.nonNegativeInt.map(die => die % 6 + 1)

    def twoDiceRoll: Rand[Int] =
      dieRoll.map2(dieRoll)(_ + _)

    // Some manuel rolls to start off
    println("\nManually roll 2 dice 4 times:")
    val (twoDiceRoll1, rngR1) = twoDiceRoll.run(rngD)
    val (twoDiceRoll2, rngR2) = twoDiceRoll.run(rngR1)
    val (twoDiceRoll3, rngR3) = twoDiceRoll.run(rngR2)
    val (twoDiceRoll4, rngR4) = twoDiceRoll.run(rngR3)
    print("twoDiceRoll1 = "); println(twoDiceRoll1)
    print("twoDiceRoll2 = "); println(twoDiceRoll2)
    print("twoDiceRoll3 = "); println(twoDiceRoll3)
    print("twoDiceRoll4 = "); println(twoDiceRoll4)

    // Auto-roll some dice
    println("\nAuto roll 2 dice 10 times:")

    val diceRolls10 = List.fill(10)(twoDiceRoll)
    val rollSome10 = Rand.sequence(diceRolls10)
    print("rollSome10.extract(rngD) = ");
    println(rollSome10.extract(rngD))

    // Test how stack-safe is sequence.
    def averageDiceRoll(numRolls: Int): Rand[Double] = {
      val diceRolls = Rand.sequence(List.fill(numRolls)(twoDiceRoll))
      diceRolls map { _.sum.toDouble/numRolls }
    }
    
    println("\nAuto roll 2 dice large number of times and find average:")
    var myPair: (Double,RNG) = (0.0, rng42)
    for (numToRoll <- List(10, 10, 10, 10, 10, 500, 500, 500, 500, 500,
                           2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000)) {
      myPair = averageDiceRoll(numToRoll).run(myPair._2)
      println("Average of " + numToRoll + " two dice rolls is " + myPair._1)
    }

    // Look at ints again.
    println("\nLook at ints again:\n")

    print("Compare with different RNGs:")
    val tenInts1 = RNG.ints(10).extract(rng42)
    val tenInts2 = RNG.ints(10).extract(rng666)
    print("\n10 Ints: "); tenInts1.foreach(x => print(x + " "))
    print("\n10 Ints: "); tenInts2.foreach(x => print(x + " "))

    print("\n\nCompare different syntax:")
    print("\n10 Ints: ")
    RNG.ints(10).extract(rng666).foreach(x => print(x + " "))
    print("\n10 Ints: ")
    RNG.ints(10).extract(rng666) foreach {x => print(x + " ")}

    // Test nonNegativeLessThan

    def baz(num: Int, lt: Int): Rand[List[Int]] = 
      Rand.sequence(List.fill(num)(RNG.nonNegativeLessThan(lt)))

    print("\n100 random non-neg Ints less than 20")
    println(" (using nonNegativeLessThan):")
    for (ii <- baz(100, 10).extract(rng42)) {print(ii + " ")}

    print("\n50 random non-neg Ints less than 200000000")
    println(" (using nonNegativeLessThan):")
    for (ii <- baz(50, 200000000).extract(rng42)) {println(ii)}

    // Test map and map2
    println("\nTest map and map2 by throwing dice:\n")

    def dieRollFM: Rand[Int] =
      RNG.nonNegativeLessThan(6).map(_ + 1)

    def twoDiceRollFM: Rand[Int] =
      dieRollFM.map2(dieRollFM)(_ + _)

    def throwSome(num: Int): Rand[List[Int]] =
      Rand.sequence(List.fill(num)(twoDiceRollFM))

    print("throwSome(20).extract(rng42) = ")
    println(throwSome(20).extract(rng42))

    // Lets try a for comprehension:

    /** Return a random action that will
     *  product a List of length less than lt
     *  and whose values are bounded by the
     *  List's length.
     */
    def makeRandList1(lt: Int): Rand[List[Int]] = for {
      n   <- RNG.nonNegativeLessThan(lt)
      d   <- RNG.nonNegativeLessThan(n)
      ns  <- RNG.ints(n)
    } yield ns map { _.abs % (d + 1) }

    // Repeat without the for comprehension

    def makeRandList2(lt: Int): Rand[List[Int]] =
      RNG.nonNegativeLessThan(lt) flatMap { n =>
        RNG.nonNegativeLessThan(n) flatMap { d =>
          RNG.ints(n) map { ns =>
            ns map { _.abs % (d + 1) }
          }
        }
      }

    println("\nMake some length/value bounded List[Int]:")

    val myRandListFor10 = makeRandList1(10)
    val myRandListFlatmap10 = makeRandList2(10)
    val myRandListFor20 = makeRandList1(20)
    val myRandListFlatmap20 = makeRandList2(20)

    print("\nmyRandListFor10.extract(rng42) = ")
    println(myRandListFor10.extract(rng42))
    print("myRandListFlatmap10.extract(rng42) = ")
    println(myRandListFlatmap10.extract(rng42))

    print("\nmyRandListFor10.extract(rng666) = ")
    println(myRandListFor10.extract(rng666))
    print("myRandListFlatmap10.extract(rng666) = ")
    println(myRandListFlatmap10.extract(rng666))

    print("\nmyRandListFor20.extract(rng42) = ")
    println(myRandListFor20.extract(rng42))
    print("myRandListFlatmap20.extract(rng42) = ")
    println(myRandListFlatmap20.extract(rng42))

    print("\nmyRandListFor20.extract(rng666) = ")
    println(myRandListFor20.extract(rng666))
    print("myRandListFlatmap20.extract(rng666) = ")
    println(myRandListFlatmap20.extract(rng666))

    println()

  }
}
