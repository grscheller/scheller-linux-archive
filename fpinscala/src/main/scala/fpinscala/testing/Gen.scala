/** Property based testing package.
 *
 *    
 *
 */
package fpinscala.testing

import fpinscala.state.rand.{Rand,RNG,LCG}
import Prop._
import Gen._

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

case class Gen[A](sample: Rand[A])

object Gen {

  /** A "lazy" unit which always generates the same value. */
  def unit[A](a: => A): Gen[A] = Gen(Rand.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Rand.exclusiveIntRange(start, stopExclusive))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(Rand.sequence(List.fill(n)(g.sample)))

  def boolean: Gen[Boolean] = Gen(Rand.boolean)

}

