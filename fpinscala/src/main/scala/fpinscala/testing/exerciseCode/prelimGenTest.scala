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

object prelimGenTest {

  val rng1: RNG = LCG(1)
  val rng2: RNG = LCG(2)
  val rng3: RNG = LCG(3)

  def main(args: Array[String]): Unit = {

    val gen20to30 = Gen.choose(20, 31)

    // Spit one value out
    print("\ngen20to30.sample(rng1) = ")
    println(gen20to30.sample(rng1))

    // Spit out 10 values
    println("\nTen random values from 20 to 30:")
    for (ii <- Gen.listOfN(10, gen20to30).sample(rng1)) {
      println(ii)
    }

    // Spit out 10 random Booleans
    println("\nTen random boolean values:")
    for (ii <- Gen.listOfN(10, Gen.boolean).sample(rng1)) {
      println(ii)
    }

    // Generate 10 random pairs
    def genPair[A](g: Gen[A]): Gen[(A,A)] =
      Gen.listOfN(2, g) map { l => (l(0), l(1)) }

    println("\nTen random pairs of Ints from 20 to 30:")
    for (pairInt <- Gen.listOfN(10, genPair(gen20to30)) sample rng1) {
      println(pairInt)
    }

    // Test unit
    println("\nGenerate a slow 42 ten time:")
    val genFortyTwo = Gen.unit({Thread.sleep(1000); 42})
    for (ii <- Gen.listOfN(10, genFortyTwo) sample rng1) {
      println(ii)
    }

    // Gen[Option[A]] from Gen[A]
    def genMin(count: Int)(g: Gen[Int]): Gen[Option[Int]] =
      Gen.listOfN(count, g) map {
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

    println()

  }
}
