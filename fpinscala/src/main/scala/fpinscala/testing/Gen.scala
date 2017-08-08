/** Property based testing package.
 *
 *   A library modeled after ScalaCheck's
 *   interface and behavior.  ScalaCheck itself
 *   is modeled after Haskell's QuickCheck.
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

case class Gen[A](sample: Rand[A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen {sample flatMap { a => f(a).sample }}

  def map[B](f: A => B): Gen[B] = Gen {sample map f}

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen {sample.map2(g.sample)(f)}

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n =>
      Gen {Rand.sequence(List.fill(n)(sample))}
    }

  def indexedSeqOfN(size: Gen[Int]): Gen[IndexedSeq[A]] =
    size flatMap { n =>
      Gen {Rand.sequenceIndexedSeq(IndexedSeq.fill(n)(sample))}
    }

}

object Gen {

  /** A "lazy" unit which always generates the same value. */
  def unit[A](a: => A): Gen[A] = Gen(Rand.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Rand.exclusiveIntRange(start, stopExclusive))

  def boolean: Gen[Boolean] = Gen(Rand.boolean)

}
