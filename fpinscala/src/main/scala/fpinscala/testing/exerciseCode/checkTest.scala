/** Package to test the fpinscala.testing package
 *
 *  Test the Prop.check companion object method
 *  in the fpinscala.testing package.
 *
 */
package fpinscala.chap08.testing

import fpinscala.testing.{Gen,SGen,Prop}
import fpinscala.state.rand.{Rand,RNG,LCG}

object checkTest {

  val rng0: RNG = LCG(System.currentTimeMillis)
  val rng1: RNG = LCG(3141592653589793L)
  val rng2: RNG = LCG(2718281828459045L)
  val rng3: RNG = LCG(1234567890L)

  def main(args: Array[String]): Unit = {

    import Prop._

    println("\n\nTest the predicates true and false.\n")

    println("Check if true is true:")
    run(check(true))

    println("Check if false is true:")
    run(check(false))

    println("Check 4 separate properties:")
    println("run(check(true) && check(true) && check(false) && check(true)):")
    run(check(true) && check(true) && check(false) && check(true))

    println("Check a single property:")
    println("run(check(true && true && false && true):")
    run(check(true && true && false && true))

    println("Check 4 separate true properties:")
    println("run(check(true) && check(true) && check(true) && check(true)):")
    run(check(true) && check(true) && check(true) && check(true))

    println("Check 4 separate false properties:")
    print("run(check(false) && check(false) ")
    println("&& check(false) && check(false)):")
    run(check(false) && check(false) && check(false) && check(false))

    // Mix in some other properties too.

    type RollValues = Int
    type DiceRoll = Gen[RollValues]
    val dieRoll: DiceRoll = Gen.choose(1, 7)
    val rollSomeDice: SGen[RollValues] = dieRoll.listOf1 map {
                                           _.foldLeft(0)(_ + _)
                                         }
    val dieRollsLT30: Prop = forAll(rollSomeDice) { _ < 30 }

    // Provable Contrived property - change to an exhaustive test later
    val check100DieRolls: Prop = check {
      Gen.sampleStream(dieRoll)(rng0) take 100 forAll {
        die => die >= 1 && die <= 6
      }
    }

    println("\nSee if 4 or less dice rolls sum less than 30.\n")

    println("run(dieRollsLT30 && check100DieRolls):")
    run(dieRollsLT30 && check100DieRolls, 4, 2000)

    println("run(check100DieRolls && dieRollsLT30):")
    run(check100DieRolls && dieRollsLT30, 4, 2000)

    println("run(dieRollsLT30):")
    run(dieRollsLT30, 4, 2000)

    println("run(check100DieRolls):")
    run(check100DieRolls, 4, 2000)

    println("\nSee if 10 or less dice rolls sum less than 30.\n")

    println("run(dieRollsLT30 && check100DieRolls, 10, 2000):")
    run(dieRollsLT30 && check100DieRolls, 10, 2000)

    println("run(check100DieRolls && dieRollsLT30, 10, 2000):")
    run(check100DieRolls && dieRollsLT30, 10, 2000)

    println("run(dieRollsLT30, 10, 2000):")
    run(dieRollsLT30, 10, 2000)

    println("run(check100DieRolls, 10, 2000):")
    run(check100DieRolls, 10, 2000)

    println()

  }

}
