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
import fpinscala.state.rand.{Rand,RNG,LCG}
import scala.util.{Try, Success, Failure}
import scala.collection.mutable

object fpinScalaCheckTest {

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
    val genFortyTwo = Gen.unit({Thread.sleep(500); 42})
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

    val numTrials = 200000
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

    // Test Prop.forAll method

    println("\nTest a true and a false Prop:")
    val twoDiceTrueProp  = Prop.forAll(twoDiceRoll)(ii => ii > 1 && ii < 13)
    val twoDiceFalseProp = Prop.forAll(twoDiceRoll)(ii => ii > 2 && ii < 12)

    print("twoDiceTrueProp.run(100, rng1): ")
    println(twoDiceTrueProp.run(100, rng1))
    print("twoDiceTrueProp.run(100, rng2): ")
    println(twoDiceTrueProp.run(100, rng2))
    print("twoDiceTrueProp.run(100, rng3): ")
    println(twoDiceTrueProp.run(100, rng3))
    print("twoDiceFalseProp.run(100, rng1): ")
    println(twoDiceFalseProp.run(100, rng1))
    print("twoDiceFalseProp.run(100, rng2): ")
    println(twoDiceFalseProp.run(100, rng2))
    print("twoDiceFalseProp.run(100, rng3): ")
    println(twoDiceFalseProp.run(100, rng3))

    println("\nCombine true and false Procs with && and ||:")
    val twoDiceTrueProp1  = Prop.forAll(twoDiceRoll)(_ > 1)
    val twoDiceTrueProp2  = Prop.forAll(twoDiceRoll)(_ < 13)
    val twoDiceFalseProp1 = Prop.forAll(twoDiceRoll)(_ > 2)
    val twoDiceFalseProp2 = Prop.forAll(twoDiceRoll)(_ < 12)

    val procTrueAndTrue = twoDiceTrueProp1 && twoDiceTrueProp2
    val procTrueOrTrue = twoDiceTrueProp1 || twoDiceTrueProp2
    val procTrueAndFalse = twoDiceTrueProp1 && twoDiceFalseProp2
    val procFalseAndTrue = twoDiceFalseProp1 && twoDiceTrueProp2
    val procTrueOrFalse = twoDiceTrueProp1 || twoDiceFalseProp2
    val procFalseOrTrue = twoDiceFalseProp1 || twoDiceTrueProp2
    val procFalseAndFalse = twoDiceFalseProp1 && twoDiceFalseProp2
    val procFalseOrFalse = twoDiceFalseProp1 || twoDiceFalseProp2

    print("procTrueAndTrue.run(100, rng1): ")
    println(procTrueAndTrue.run(100, rng1))
    print("procTrueAndTrue.run(100, rng2): ")
    println(procTrueAndTrue.run(100, rng2))
    print("procTrueAndTrue.run(100, rng3): ")
    println(procTrueAndTrue.run(100, rng3))
    print("procTrueOrTrue.run(100, rng1): ")
    println(procTrueOrTrue.run(100, rng1))
    print("procTrueOrTrue.run(100, rng2): ")
    println(procTrueOrTrue.run(100, rng2))
    print("procTrueOrTrue.run(100, rng3): ")
    println(procTrueOrTrue.run(100, rng3))

    print("procTrueAndFalse.run(100, rng1): ")
    println(procTrueAndFalse.run(100, rng1))
    print("procTrueAndFalse.run(100, rng2): ")
    println(procTrueAndFalse.run(100, rng2))
    print("procTrueAndFalse.run(100, rng3): ")
    println(procTrueAndFalse.run(100, rng3))
    print("procTrueOrFalse.run(100, rng1): ")
    println(procTrueOrFalse.run(100, rng1))
    print("procTrueOrFalse.run(100, rng2): ")
    println(procTrueOrFalse.run(100, rng2))
    print("procTrueOrFalse.run(100, rng3): ")
    println(procTrueOrFalse.run(100, rng3))

    print("procFalseAndTrue.run(100, rng1): ")
    println(procFalseAndTrue.run(100, rng1))
    print("procFalseAndTrue.run(100, rng2): ")
    println(procFalseAndTrue.run(100, rng2))
    print("procFalseAndTrue.run(100, rng3): ")
    println(procFalseAndTrue.run(100, rng3))
    print("procFalseOrTrue.run(100, rng1): ")
    println(procFalseOrTrue.run(100, rng1))
    print("procFalseOrTrue.run(100, rng2): ")
    println(procFalseOrTrue.run(100, rng2))
    print("procFalseOrTrue.run(100, rng3): ")
    println(procFalseOrTrue.run(100, rng3))

    print("procFalseAndFalse.run(100, rng1): ")
    println(procFalseAndFalse.run(100, rng1))
    print("procFalseAndFalse.run(100, rng2): ")
    println(procFalseAndFalse.run(100, rng2))
    print("procFalseAndFalse.run(100, rng3): ")
    println(procFalseAndFalse.run(100, rng3))
    print("procFalseOrFalse.run(100, rng1): ")
    println(procFalseOrFalse.run(100, rng1))
    print("procFalseOrFalse.run(100, rng2): ")
    println(procFalseOrFalse.run(100, rng2))
    print("procFalseOrFalse.run(100, rng3): ")
    println(procFalseOrFalse.run(100, rng3))

    println("\nTest a Prop which throws an exception:")
    val exceptionalProp = Prop.forAll(twoDiceRoll) {
      ii => 42/(ii - 2) < 43
    }

    print("exceptionalProp.run(10, rng1): ")
    println(exceptionalProp.run(10, rng1))
    print("exceptionalProp.run(50, rng1): ")
    println(exceptionalProp.run(50, rng1))
    print("exceptionalProp.run(100, rng1): ")
    println(exceptionalProp.run(100, rng1))

    println("\nTest a failable Prop which could throw an exception:")
    val failableExceptionalProp = Prop.forAll(twoDiceRoll) {
      ii => 42/(ii - 2) < 42
    }

    print("failableExceptionalProp.run(10, rng3): ")
    println(failableExceptionalProp.run(10, rng3))
    print("failableExceptionalProp.run(10, rng1): ")
    println(failableExceptionalProp.run(10, rng1))
    print("failableExceptionalProp.run(20, rng1): ")
    println(failableExceptionalProp.run(20, rng1))
    print("failableExceptionalProp.run(10, rng2): ")
    println(failableExceptionalProp.run(10, rng2))
    print("failableExceptionalProp.run(20, rng2): ")
    println(failableExceptionalProp.run(20, rng2))

    println("\nCombining a failable Prop with an exceptional Prop:")
    val procTrueOrExceptional = twoDiceTrueProp1 || exceptionalProp
    val procExceptionalOrTrue = exceptionalProp || twoDiceTrueProp1 
    val procTrueAndExceptional = twoDiceTrueProp1 && exceptionalProp
    val procExceptionalAndTrue = exceptionalProp && twoDiceTrueProp1 
    val procFalseOrExceptional = twoDiceFalseProp1 || exceptionalProp
    val procExceptionalOrFalse = exceptionalProp || twoDiceFalseProp1 

    print("procTrueOrExceptional.run(5, rng2): ")
    println(procTrueOrExceptional.run(5, rng2))
    print("procTrueOrExceptional.run(15, rng2): ")
    println(procTrueOrExceptional.run(15, rng2))

    print("procExceptionalOrTrue.run(5, rng2): ")
    println(procExceptionalOrTrue.run(5, rng2))
    print("procExceptionalOrTrue.run(15, rng2): ")
    println(procExceptionalOrTrue.run(15, rng2))

    print("procTrueAndExceptional.run(10, rng2): ")
    println(procTrueAndExceptional.run(10, rng2))
    print("procTrueAndExceptional.run(11, rng2): ")
    println(procTrueAndExceptional.run(11, rng2))
    print("procTrueAndExceptional.run(12, rng2): ")
    println(procTrueAndExceptional.run(12, rng2))

    print("procExceptionalAndTrue.run(5, rng2): ")
    println(procExceptionalAndTrue.run(5, rng2))
    print("procExceptionalAndTrue.run(15, rng2): ")
    println(procExceptionalAndTrue.run(15, rng2))

    print("procExceptionalOrFalse.run(15, rng2): ")
    println(procExceptionalOrFalse.run(15, rng2))
    print("procFalseOrExceptional.run(15, rng2): ")
    println(procFalseOrExceptional.run(15, rng2))

    println("\nDone!\n")

  }

}
