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

  val rng30 = LCG(30)
  val rng1492 = LCG(1492)

  def main(args: Array[String]): Unit = {

    val foo = Gen.choose(42, 50)
    print("foo.sample(rng30) = "); println(foo.sample(rng30))
    print("foo.sample(rng1492) = "); println(foo.sample(rng1492))

    println()

  }
}
