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
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.exclusiveIntRange(start, stopExclusive))
  // def listOf[A](a: Gen[A]): Gen[List[A]]
  // def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}
