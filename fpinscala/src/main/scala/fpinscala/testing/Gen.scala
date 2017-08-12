/** Property based testing package.
 *
 *   A library modeled after ScalaCheck's
 *   interface and behavior.  ScalaCheck itself
 *   is modeled after Haskell's QuickCheck.
 *
 */
package fpinscala.testing

import fpinscala.state.rand.{Rand,RNG}
import fpinscala.laziness.Stream
import Prop._

case class Prop(run: (TestCount, RNG) => Result)

object Prop {
  type FailedCase = String
  type TestCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified( failure:   FailedCase
                      , successes: TestCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](g: Gen[A])(pred: A => Boolean): Prop = Prop {
    (n, rng) => sampleStream(g)(rng) zip Stream.from(0) take n map {
      case (a, cnt: TestCount) => try {
        if (pred(a))
          Passed
        else
          Falsified(a.toString, cnt)
      } catch {
          case e: Exception => Falsified(buildMsg(a, e), cnt)
      }
    } find(_.isFalsified) getOrElse Passed
  }

  def sampleStream[A](g: Gen[A])(rngIn: RNG): Stream[A] =
    Stream.unfold(rngIn) {
      rng => Some(g.sample.action.run(rng))
    }

  private def buildMsg[A](a: A, e: Exception): FailedCase =
    s"Test case: ${a}\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

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

  /** Pull from 2 Generators of the same type with equal likelyhod */
  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] =
    Gen { Rand.joint2(0.5)(gen1.sample, gen2.sample) }

  /** Pull 2 Generators with relative weights */
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val prob1 = g1._2.abs/(g1._2.abs + g2._2.abs)
    Gen { Rand.joint2(prob1)(g1._1.sample, g2._1.sample) }
  }

}
