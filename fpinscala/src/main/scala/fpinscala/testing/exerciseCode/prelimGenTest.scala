/** Package to test the fpinscala.testing package
 *
 *  Some preliminary testing of the of content of the
 *  testing package itself.
 *
 *  Will actually use the package itself for property
 *  based testing under the test/ source code tree.
 *
 */
package fpinscala.chap08.testing

import fpinscala.testing.{Gen,Prop}
import fpinscala.state.rand.{Rand,RNG,LCG} // Remove implementation detail.
import scala.util.{Try, Success, Failure}
import scala.collection.mutable

object prelimGenTest {

  val rng1: RNG = LCG(1)
  val rng2: RNG = LCG(2)
  val rng3: RNG = LCG(3)

  def main(args: Array[String]): Unit = {

    // Some initial type experiments, before many
    // convienence functions are defined.

    val gen20to30 = Gen.choose(20, 31)

    // Spit one value out
    print("\ngen20to30.sample(rng1) = ")
    println(gen20to30.sample(rng1))

    // Spit out 10 values
    println("\nTen random values from 20 to 30:")
    for (ii <- gen20to30.listOfN(Gen.unit(10)).sample(rng1)) {
      println(ii)
    }

    // Spit out between 5 and 10 random Booleans
    println("\n5 to 10 random boolean values:")
    for (ii <- Gen.boolean.listOfN(Gen.choose(5, 11)).sample(rng1)) {
      println(ii)
    }

    // Generate 3 to 5 random pairs of Int - using listOfN
    println("\n3 to 5 random pairs of Ints from 20 to 30 using listOfN:")

    def genPair1[A](g: Gen[A]): Gen[(A,A)] =
      g listOfN Gen.unit(2) map { l => (l(0), l(1)) }

    for (pairInt <- genPair1(gen20to30) listOfN Gen.choose(3, 6) sample rng1) {
      println(pairInt)
    }

    // Generate 3 to 5 random pairs of Int - using map2
    println("\n3 to 5 random pairs of Ints from 20 to 30 using map2:")

    def genPair2[A](g: Gen[A]): Gen[(A,A)] = g.map2(g)((_, _)) 

    for (pairInt <- genPair2(gen20to30) listOfN Gen.choose(3, 6) sample rng1) {
      println(pairInt)
    }

    // Test unit
    println("\nGenerate a slow 42 ten time:")
    val genFortyTwo = Gen.unit({Thread.sleep(1000); 42})
    for (ii <- genFortyTwo.listOfN(Gen.unit(10)) sample rng1) {
      println(ii)
    }

    // Gen[Option[A]] from Gen[A]
    def genMin(count: Int)(g: Gen[Int]): Gen[Option[Int]] =
      g listOfN Gen.unit(count) map {
        (l: List[Int]) => Try(l.min).toOption
      }

    println("\nGenerate Gen[Option[Int]] from a Gen[Int]:")
    for (rng <- Seq(rng1, rng2, rng3)) {
      print(s"genMin(3)(gen20to30) sample ${rng}) = ")
      println(genMin(3)(gen20to30) sample rng)
      print(s"genMin(0)(gen20to30) sample ${rng}) = ")
      println(genMin(0)(gen20to30) sample rng)
    }

    // Gen[A] from Gen[Option[A]]
    val some10: Option[Int] = Some(10)
    val noInt: Option[Int] = None
    val genInt10 = Gen.unit(some10) 
    val genIntNone = Gen.unit(noInt) 

    println("\nGenerate Gen[Int] from a Gen[Option[Int]]:")
    for (rng <- Seq(rng1, rng2, rng3)) {
      print(s"genInt10 sample ${rng} getOrElse 42 = ")
      println(genInt10 sample rng getOrElse 42)
      print(s"genIntNone sample ${rng} getOrElse 42 = ")
      println(genIntNone sample rng getOrElse 42)
    }

    // Generate some random strings
    
    val genLowerCase = Gen.choose(97, 123) map (_.toChar)
    val genUpperCase = Gen.choose(65,  90) map (_.toChar)
    val genDigit     = Gen.choose(48,  58) map (_.toChar)

    val genRandomChar = 
      Gen(Rand.joint3(26.0/53.0, 26.0/53.0)( genLowerCase.sample
                                           , genUpperCase.sample
                                           , genDigit.sample ))

    def genStringN(n: Int): Gen[String] = 
      genRandomChar listOfN Gen.unit(n) map { _.mkString }

    val gen10RandomStrLt30: Gen[List[String]] =
      Gen.choose(0, 31) flatMap (genStringN _) listOfN Gen.unit(10) 

    println("\nGenerate 10 random strings of random lengths < 30:")
    for (str <- gen10RandomStrLt30 sample rng1) println(str)

    // Test Gen.indexedSeqOfN class method
    type DiceRoll = Gen[Int]
    val dieRoll: DiceRoll = Gen.choose(1, 7)
    val rollFiveDice: DiceRoll = 
      dieRoll indexedSeqOfN Gen.unit(5) map {_.sum}
    val rollFiveDiceTenToTwentyTimes =
      rollFiveDice indexedSeqOfN Gen.choose(10, 21)

    println("\nThrow 5 dice between 10 and 20 times:")
    for (throwSome <- rollFiveDiceTenToTwentyTimes sample rng1) {
      println(throwSome)
    }
    println("\nAgain, throw 5 dice between 10 and 20 times:")
    for (throwSome <- rollFiveDiceTenToTwentyTimes sample rng2) {
      println(throwSome)
    }

    val numTrials = 1000000
    val freqFiveDiceRolls = mutable.Map[Int, Int]()
    for (ii <- 5 to 30) { freqFiveDiceRolls += (ii -> 0) }
    for (throw5 <- rollFiveDice indexedSeqOfN Gen.unit(numTrials) sample rng1) {
      freqFiveDiceRolls(throw5) += 1
    }
    println(s"\nFrequency of 5 dice rolls, ${numTrials} samples:")
    for (ii <- 5 to 30) 
      if (ii < 10)
        println(s" ${ii} -> ${freqFiveDiceRolls(ii)}")
      else
        println(s"${ii} -> ${freqFiveDiceRolls(ii)}")

    // Test Gen.map2 class method
    val twoDiceRoll = dieRoll.map2(dieRoll) { _ + _ }
    val rollTwoDiceTenTimes = twoDiceRoll listOfN Gen.unit(20)
    println("\nRoll 2 dice 20 times:")
    println(rollTwoDiceTenTimes sample rng3)

    println()

  }

}
