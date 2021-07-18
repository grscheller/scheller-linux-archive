/** Package to test the fpinscala.testing package
  *
  *  Preliminary testing of the of the SGen case class
  *  in the fpinscala.testing package.
  */
package fpinscala.chap08.testing

import fpinscala.testing.{Gen, SGen, Prop}
import fpinscala.state.rand.{Rand, RNG, LCG}

import Prop._

object sgenTest {

  val rng1: RNG = LCG(3141592653589793L)
  val rng2: RNG = LCG(2718281828459045L)
  val rng3: RNG = LCG(1234567890L)
  val rng4: RNG = LCG(42L)

  def main(args: Array[String]): Unit = {

    // Test Gen.indexedSeqOfN class method
    type DiceRoll = Gen[Int]
    val dieRoll: DiceRoll = Gen.choose(1, 7)
    val rollTwoDice: DiceRoll = dieRoll.map2(dieRoll)(_ + _)
    val rollSome = rollTwoDice.listOf

    println("\nRoll a pair of dice 5 times:")
    for (throwTwo <- rollSome(5) sample rng1) {
      println(throwTwo)
    }
    println("\nRoll a pair of dice 3 times:")
    for (throwTwo <- rollSome(3) sample rng2) {
      println(throwTwo)
    }

    println("\nThrow 2 dice to decide how many times to throw 1:")
    val roll2toRoll1 = rollSome flatMap { ns => dieRoll.listOf(ns.min) }
    for (nn <- Seq(1, 2, 3, 4, 5, 10, 20, 35, 50, 100)) {
      println(s"Least of ${nn} double rolls:")
      println(roll2toRoll1(nn) sample rng1)
      println(roll2toRoll1(nn) sample rng2)
      println(roll2toRoll1(nn) sample rng3)
      println(roll2toRoll1(nn) sample rng4)
      println()
    }

    // Example based on one from book (section 8.4.1)

    // Generates nonempty lists of (ii: Int) where -10 <= ii <= 10
    val smallIntList = Gen.choose(-10, 11).listOf1

    // A property that should obviously be true (for nonempty lists)
    val maxProp = forAll(smallIntList) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    println("Test a true property:\n")
    run(maxProp)

    // A test that will fail for the case of a list containing -10 and 10.
    val falseProp = forAll(smallIntList) { ns =>
      val min = ns.min
      !ns.exists(_ > min + 19)
    }

    print("\nTest a property that occasionally fails,")
    println(" using different test parameters:\n")

    run(falseProp, 100, 100); println()
    run(falseProp); println()
    run(falseProp, 100, 10000); println()
    run(falseProp, 1000, 20); println()
    run(falseProp, 13, 37); println()

    // Example of a seldom failing test with fewer
    // test cases but larger datastructures.

    // Generates nonempty lists of (ii: Int) where -1000 <= ii <= 1000
    val largeIntList = Gen.choose(-10000, 10001).listOf1

    // A property that fails rarely
    val seldomFailProp1 = forAll(largeIntList) { ns =>
      !ns.exists(_ == 42)
    }

    // A property that fails rarely
    val seldomFailProp2 = forAllPow2Banded(largeIntList) { ns =>
      !ns.exists(_ == 42)
    }

    println("\nTest a property that seldom fails with Prop.forAll,")
    println("run(seldomFailProp1, 10, 10000):")
    run(seldomFailProp1, 10, 10000)

    println("\nTest a property that seldom fails with Prop.forAllPow2Banded,")
    println("run(seldomFailProp2, 10, 100):")
    run(seldomFailProp2, 10, 1000)

    println()

  }
}
