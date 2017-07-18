/** Package to test the fpinscala.testing package
 *
 *  Some preliminary testing of the of content of the
 *  testing package itself.
 *
 *  Will actually use the package under for property
 *  based testing under the test/ source code tree.
 *
 */
package fpinscala.chap08.testing

import fpinscala.testing.{Gen,Prop}
import fpinscala.state.rand.{Rand,RNG,LCG} // Remove implementation detail.

object prelimGenTest {

  val rng30: RNG = LCG(30)

  def main(args: Array[String]): Unit = {

    val foo = Gen.choose(20, 31)

    // Spit one value out
    print("foo.sample(rng30) = "); println(foo.sample(rng30))

    // Now imperitively, turn the crank
    var pair = (0, rng30)
    for (ii <- 1 to 100) {
      pair = foo.sample.run(pair._2)
      println(pair._1)
    }

    println()

  }
}
