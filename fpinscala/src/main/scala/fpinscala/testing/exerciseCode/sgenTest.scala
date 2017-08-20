/** Package to test the fpinscala.testing package
 *
 *  Preliminary testing of the of the SGen case class
 *  in the testing package.
 *
 *  Will actually use the package itself for property
 *  based testing under test/ source code trees.
 *
 */
package fpinscala.chap08.testing

import fpinscala.testing.{Gen,SGen,Prop}
import fpinscala.state.rand.{Rand,RNG,LCG}

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
    for (nn <- Seq(1,2,3,4,5,10,20,35,50,100)) {
      println(s"Least of ${nn} double rolls:")
      println(roll2toRoll1(nn) sample rng1)
      println(roll2toRoll1(nn) sample rng2)
      println(roll2toRoll1(nn) sample rng3)
      println(roll2toRoll1(nn) sample rng4)
      println()
    }

    println()

  }

}
