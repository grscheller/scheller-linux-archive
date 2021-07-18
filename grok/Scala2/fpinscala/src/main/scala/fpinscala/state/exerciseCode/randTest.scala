/** Package to test the fpinscala.rand package and
  *  indirectly fpinscala.state
  *
  *  Deliberately not importing fpinscala.state
  *  since I feel it is an implementation detail
  *  of the fpinscala.state.rand package.
  *
  *  Since I changed the nature of Rand from an
  *  "is a state action" to a "has a state action",
  *  I am dropping the lower level state run tests.
  */
package fpinscala.chap06.state.rand

import fpinscala.state.rand.{Rand, RNG, LCG}

object randTest {

  val rng42 = LCG(42)
  val rng666 = LCG(666)
  val rng777 = LCG(777)

  def main(args: Array[String]): Unit = {

    print("\nrng42 = "); println(rng42)

    print("Rand.nonNegInt(rng42)  = ")
    println(Rand.nonNegInt(rng42))

    // Test map and map2
    println("\nTest map and map2 by throwing dice:")

    def dieRoll: Rand[Int] =
      Rand.nonNegInt map { die => die % 6 + 1 }

    def twoDiceRoll: Rand[Int] =
      (dieRoll map2 dieRoll) { _ + _ }

    // Roll some dice
    println("Auto rolling 2 dice 10 and 20 times -")

    val diceRolls10 = List.fill(10)(twoDiceRoll)
    val rollSome10 = Rand.sequence(diceRolls10)
    val diceRolls20 = List.fill(20)(twoDiceRoll)
    val rollSome20 = Rand.sequence(diceRolls20)
    print("rollSome10(rng42) = ");
    println(rollSome10(rng42))
    print("rollSome20(rng777) = ");
    println(rollSome20(rng777))

    // Test how stack-safe is sequence.
    println("\nTest stack-safety of Rand.sequence:")
    def averageTwoDiceRoll(numRolls: Int): Rand[Double] = {
      val diceRolls = Rand.sequence(List.fill(numRolls)(twoDiceRoll))
      diceRolls map { _.sum.toDouble / numRolls }
    }

    print("averageTwoDiceRoll(10)(rng42) = ")
    println(averageTwoDiceRoll(10)(rng42))
    print("averageTwoDiceRoll(10)(rng666) = ")
    println(averageTwoDiceRoll(10)(rng666))
    print("averageTwoDiceRoll(10)(rng777) = ")
    println(averageTwoDiceRoll(10)(rng777))
    print("averageTwoDiceRoll(100)(rng42) = ")
    println(averageTwoDiceRoll(100)(rng42))
    print("averageTwoDiceRoll(100)(rng666) = ")
    println(averageTwoDiceRoll(100)(rng666))
    print("averageTwoDiceRoll(100)(rng777) = ")
    println(averageTwoDiceRoll(100)(rng777))
    print("averageTwoDiceRoll(2000)(rng42) = ")
    println(averageTwoDiceRoll(2000)(rng42))
    print("averageTwoDiceRoll(2000)(rng666) = ")
    println(averageTwoDiceRoll(2000)(rng666))
    print("averageTwoDiceRoll(2000)(rng777) = ")
    println(averageTwoDiceRoll(2000)(rng777))
    print("averageTwoDiceRoll(50000)(rng42) = ")
    println(averageTwoDiceRoll(50000)(rng42))
    print("averageTwoDiceRoll(50000)(rng666) = ")
    println(averageTwoDiceRoll(50000)(rng666))
    print("averageTwoDiceRoll(50000)(rng777) = ")
    println(averageTwoDiceRoll(50000)(rng777))

    // Look at ints.
    println("\nLook at ints:")

    print("Compare with different RNGs -")
    val tenInts1 = Rand.ints(10)(rng42)
    val tenInts2 = Rand.ints(10)(rng666)
    print("\n10 Ints: ")
    tenInts1 foreach { x => print(s"$x ") }
    print("\n10 Ints: ")
    tenInts2 foreach { x => print(s"$x ") }

    print("\n\nCompare different syntax -")
    print("\n10 Ints: ")
    Rand.ints(10)(rng666).foreach(x => print(s"$x "))
    print("\n10 Ints: ")
    Rand.ints(10)(rng666) foreach { x =>
      print(s"$x ")
    }

    // Test Rand.nonNegIntLessThan
    print("\n\nTest Rand.nonNegIntLessThan and ")
    println("Rand.sequence:")

    def baz(num: Int, lt: Int): Rand[List[Int]] =
      Rand.sequence(List.fill(num)(Rand.nonNegIntLessThan(lt)))

    print("100 random non-neg Ints less than 10")
    println(" (using Rand.nonNegIntLessThan) -")
    for (ii <- baz(100, 10)(rng42)) { print("$ii ") }

    print("\n\n50 random non-neg Ints less than 200000000")
    println(" (using Rand.nonNegIntLessThan) -")
    for (ii <- baz(50, 200000000)(rng42)) { println(ii) }

    // Test map and map2
    println("\nTest Rand.sequence by throwing dice:")

    def throwSome(num: Int): Rand[List[Int]] =
      Rand.sequence(List.fill(num)(twoDiceRoll))

    print("throwSome(20)(rng42) = ")
    println(throwSome(20)(rng42))

    // Lets try a for comprehension:
    println("\nTest Rand for comprehensions:")

    /** Return a random action that will
      *  product a List of length less than lt
      *  and whose values are bounded by a
      *  random int less than the List's length.
      *
      *  This function is a bit contrived.
      */
    def makeRandList1(lt: Int): Rand[List[Int]] = for {
      n <- Rand.nonNegIntLessThan(lt)
      d <- Rand.nonNegIntLessThan(n)
      ns <- Rand.ints(n)
    } yield ns map { _.abs % (d + 1) }

    // Repeat without the for comprehension

    def makeRandList2(lt: Int): Rand[List[Int]] =
      Rand.nonNegIntLessThan(lt) flatMap { n =>
        Rand.nonNegIntLessThan(n) flatMap { d =>
          Rand.ints(n) map { ns =>
            ns map { _.abs % (d + 1) }
          }
        }
      }

    println("Make some length/value bounded List[Int] -")

    val myRandListFor10 = makeRandList1(10)
    val myRandListFlatmap10 = makeRandList2(10)
    val myRandListFor20 = makeRandList1(20)
    val myRandListFlatmap20 = makeRandList2(20)

    print("myRandListFor10(rng42) = ")
    println(myRandListFor10(rng42))
    print("myRandListFlatmap10(rng42) = ")
    println(myRandListFlatmap10(rng42))

    print("\nmyRandListFor10(rng666) = ")
    println(myRandListFor10(rng666))
    print("myRandListFlatmap10(rng666) = ")
    println(myRandListFlatmap10(rng666))

    print("\nmyRandListFor20(rng42) = ")
    println(myRandListFor20(rng42))
    print("myRandListFlatmap20(rng42) = ")
    println(myRandListFlatmap20(rng42))

    print("\nmyRandListFor20(rng666) = ")
    println(myRandListFor20(rng666))
    print("myRandListFlatmap20(rng666) = ")
    println(myRandListFlatmap20(rng666))

    println()

  }
}
