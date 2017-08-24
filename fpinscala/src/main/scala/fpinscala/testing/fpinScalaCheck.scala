/** Property based testing package.
 *
 *   A library modeled after ScalaCheck's
 *   interface and behavior.  ScalaCheck itself
 *   is modeled after Haskell's QuickCheck.
 *
 */
package fpinscala.testing

import fpinscala.state.rand.{Rand,RNG,LCG}
import fpinscala.laziness.Stream
import scala.language.implicitConversions

import Gen.unsized
import Prop._

case class Prop(run: (MaxSize, TestCount, RNG) => Result) {

  // The && and || combinators pass the same RNG to the run
  // methods of both Props. 
  //
  // Assuming that the Props are not "ad hoc" and were created
  // by the forAll method, and if both Props come from the same
  // underlying Gen, then they will generate the same sequence
  // of random values.

  /** Combine two Prop's, both must hold */
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case x      => x
    }
  }

  /** Combine two Prop's, at least one must hold */
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(failure, _) => p.tag(failure).run(max, n, rng)
      case _                     => Passed
    }
  }

  private def tag(fail1: FailedCase) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(fail2, cnt) => Falsified(s"${fail1};${fail2}", cnt)
      case _                     => Passed
    }
  }

  // Initial convenience functions used in prototyping library.
  // Not primary user interface, kept around to keep the initial
  // exercisaCode scripts working.

  def apply(max: MaxSize, cnt: TestCount, rng: RNG): Result = run(max, cnt, rng)
  def apply(cnt: TestCount, rng: RNG): Result = run(cnt, cnt, rng)

}

object Prop {
  type FailedCase = String
  type TestCount = Int
  type MaxSize   = Int

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

  val passedProp = Prop { (_,_,_) => Passed }

  private def buildMsg[A](a: A, e: Exception): FailedCase =
    s"\n>>> Test case with value: ${a}" +
    s"\n>>> generated an exception: ${e.getMessage}" +
    s"\n>>> stack trace:\n> ${e.getStackTrace.mkString("\n> ")}"

  def apply(run: (TestCount,RNG) => Result): Prop =
    Prop { (_, n, rng) => run(n, rng) }

  // Uses the above apply method
  def forAll[A](g: Gen[A])(pred: A => Boolean): Prop = Prop {
    (n, rng) => Gen.sampleStream(g)(rng) zip Stream.from(0) take n map {
      case (a, cnt: TestCount) =>
        try {
            if (pred(a))
              Passed
            else
              Falsified(a.toString, cnt)
        } catch {
            case e: Exception => Falsified(buildMsg(a, e), cnt)
        }
    } find(_.isFalsified) getOrElse Passed
  }

  def forAll[A](sg: SGen[A])(pred: A => Boolean): Prop = forAll(sg(_))(pred)

  def forAll[A](f: Int => Gen[A])(pred: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1))/max
      val props: Stream[Prop] =
        Stream.from(0) take (n.min(max)+1) map { i => forAll(f(i))(pred) }
      val prop: Prop = (props map { p => Prop { (max1, _, rng1) =>
          p.run(max1, casesPerSize, rng1)
        }
      }).foldLeft(passedProp)(_ && _)
      prop.run(max, n, rng)
  }

  /** Primary user interface to launch testing.
   *
   *  @param p Property (Prop) to be tested.
   *  @param maxsize Maximum "size" to be tested.  Here size could mean
   *                 the size of the data structures or values within, depends
   *                 how the Prop is defined.
   *  @param testCases Number of testcases that need to be be conducted until
   *                   the Prop is considered "passed."
   *  @param rng The source of randomness.  Pass a definite value for
   *             reproducible tests.
   *  @note Test cases start small and work their way larger.
   */
  def run( p: Prop
         , maxSize: Int = 100
         , testCases: Int = 100
         , rng: RNG = LCG(System.currentTimeMillis) ): Unit =

    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after ${n} passed tests:${msg}")
      case Passed =>
        println(s"+ OK, passed ${testCases} tests.")
    }

}

case class Gen[+A](sample: Rand[A]) {

  import Gen._

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen {sample flatMap { a => f(a).sample }}

  def map[B](f: A => B): Gen[B] = Gen {sample map f}

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen {sample.map2(g.sample)(f)}

  /** Generates lists of type List[A] with random length */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n =>
      Gen {Rand.sequence(List.fill(n)(sample))}
    }

  /** Generate lists of type List[A] with a definite length */
  def listOfN(size: Int): Gen[List[A]] = listOfN(unit(size))

  /** SGen to produce lists Gens of List[A] of a definite length */
  def listOf: SGen[List[A]] = SGen { n => listOfN(n) }

  /** Like listOf, but don't produce empty lists */
  def listOf1: SGen[List[A]] = SGen { n => listOfN(n.max(1)) }

  def indexedSeqOfN(size: Gen[Int]): Gen[IndexedSeq[A]] =
    size flatMap { n =>
      Gen {Rand.sequenceIndexedSeq(IndexedSeq.fill(n)(sample))}
    }

  def indexedSeqOfN(size: Int): Gen[IndexedSeq[A]] = indexedSeqOfN(unit(size))
  
  def indexedSeqOf: SGen[IndexedSeq[A]] = SGen { n => indexedSeqOfN(n) }
  
  def indexedSeqOf1: SGen[IndexedSeq[A]] =
    SGen { n => indexedSeqOfN(n.max(1)) }

  /** Convenience function to convert to an SGen.
   *
   *  Handles some edge cases the implicit def does
   *  not work without type annotation hints.
   */
  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  /** A "lazy" unit which always generates the same value. */
  def unit[A](a: => A): Gen[A] = Gen(Rand.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Rand.exclusiveIntRange(start, stopExclusive))

  def boolean: Gen[Boolean] = Gen(Rand.boolean)

  /** Pull from 2 Generators of the same type with equal likelihood */
  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] =
    Gen { Rand.joint2(0.5)(gen1.sample, gen2.sample) }

  /** Pull from 2 Generators with relative weights */
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val prob1 = g1._2.abs/(g1._2.abs + g2._2.abs)
    Gen { Rand.joint2(prob1)(g1._1.sample, g2._1.sample) }
  }

  def sampleStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng) {
      rng1 => Some(g.sample.action.run(rng1))
    }

  // Some convenience functions
  def listOf[A](g: Gen[A]): SGen[List[A]] = g.listOf
  def listOf1[A](g: Gen[A]): SGen[List[A]] = g.listOf1
  def indexedSeqOf[A](g: Gen[A]): SGen[IndexedSeq[A]] = g.indexedSeqOf
  def indexedSeqOf1[A](g: Gen[A]): SGen[IndexedSeq[A]] = g.indexedSeqOf1

}

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen { n =>
      forSize(n) flatMap { a => f(a).forSize(n) }
    }

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  def map2[B,C](sg: SGen[B])(f: (A,B) => C): SGen[C] = SGen {
    n => forSize(n).map2(sg.forSize(n))(f)
  }

}
