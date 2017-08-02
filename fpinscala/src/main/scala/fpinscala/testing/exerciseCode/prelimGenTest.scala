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

object prelimGenTest {

  val rng: RNG = LCG(1010)

  def main(args: Array[String]): Unit = {

    val from20to30 = Gen.choose(20, 31)

    // Spit one value out
    print("\nfrom20to30.sample(rng) = ")
    println(from20to30.sample(rng))

    // Spit out 10 values
    println("\nTen random values from 20 to 30:")
    for (ii <- Gen.listOfN(10, from20to30).sample(rng)) {
      println(ii)
    }

    // Spit out 10 random Booleans
    println("\nTen random boolean values:")
    for (ii <- Gen.listOfN(10, Gen.boolean).sample(rng)) {
      println(ii)
    }

    // Generate 10 random pairs
    def genPair[A](g: Gen[A]): Gen[(A,A)] =
      Gen(Gen.listOfN(2, g).sample map { l => (l(0), l(1)) })

    println("\nTen random pairs of Ints from 20 to 30:")
    for (pairInt <- Gen.listOfN(10, genPair(from20to30)) sample rng) {
      println(pairInt)
    }

    // Test unit
    println("\nGenerate 42 ten time:")
    val genFortyTwo = Gen.unit({Thread.sleep(1000); 42})
    for (ii <- Gen.listOfN(10, genFortyTwo) sample rng) {
      println(ii)
    }

    println()

  }
}
