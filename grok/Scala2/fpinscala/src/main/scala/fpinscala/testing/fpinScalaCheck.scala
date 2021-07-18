/** Property based testing package.
  *
  * A library modeled after ScalaCheck's
  * interface and behavior.  ScalaCheck itself
  * is modeled after Haskell's QuickCheck.
  */
package fpinscala.testing

import fpinscala.state.rand.{Rand, RNG, LCG}
import fpinscala.laziness.Stream
import scala.language.implicitConversions

import Gen.unsized
import Prop._

/** Case class representing a test of a property.
  *
  * The && and || combinators combine "tests" of properties
  * and should not be thought of as combining "predicates."
  * They pass the same RNG to the run methods of both Props.
  * If both Props are constructed from the same underlying
  * Gen or SGen, they will generate the same test cases for both.
  *
  * For && both Props must succeed for each test case.
  * For || one or another must succeed for each test case.
  */
case class Prop(run: (MaxSize, TestCount, RNG) => Result) {

  /** Combine two Prop's, both must hold */
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed =>
        p.run(max, n, rng) match {
          case Passed | Proved => Passed
          case falsified       => falsified
        }
      case Proved    => p.run(max, n, rng)
      case falsified => falsified
    }
  }

  /** Combine two Prop's, at least one must hold */
  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed =>
        p.run(max, n, rng) match {
          case Proved => Proved
          case _      => Passed
        }
      case Proved => Proved
      case Falsified(fail1, except1, m) =>
        p.run(max, n, rng) match {
          case Falsified(fail2, except2, r) =>
            Falsified(fail1 ::: fail2, except1 ::: except2, m.min(r))
          case success => success
        }
    }
  }

}

object Prop {
  type FailedCase = String
  type TestCount = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case object Proved extends Result {
    def isFalsified = false
  }
  case class Falsified(
      failures: List[FailedCase],
      exceptions: List[Option[Throwable]],
      successes: TestCount
  ) extends Result {
    def isFalsified = true
  }

  val passedProp = Prop { (_, _, _) => Passed }
  val provedProp = Prop { (_, _, _) => Proved }
  val failedProp = Prop { (_, _, _) => Falsified(List(), List(), 0) }

  def apply(run: (TestCount, RNG) => Result): Prop =
    Prop { (_, n, rng) => run(n, rng) }

  // Uses the companion object Prop.apply method
  // above to convert to a Prop case class.
  def forAll[A](g: Gen[A])(pred: A => Boolean): Prop = Prop { (n, rng) =>
    Gen.sampleStream(g)(rng) zip Stream.from(0) take n map { case (a, m) =>
      try {
        if (pred(a))
          Passed
        else
          Falsified(List(a.toString), List(None), m)
      } catch {
        case e: Exception => Falsified(List(a.toString), List(Some(e)), m)
      }
    } find (_.isFalsified) getOrElse Passed
  }

  def forAll[A](sg: SGen[A])(pred: A => Boolean): Prop = forAll(sg(_))(pred)

  def forAll[A](f: Int => Gen[A])(pred: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = 1 + n / max.max(1)
      val props: Stream[Prop] =
        Stream.from(0) take (n.min(max)) map { i => forAll(f(i))(pred) }
      val prop: Prop = (props map { p =>
        Prop { (max1, _, rng1) =>
          p.run(max1, casesPerSize, rng1)
        }
      }).foldLeft(provedProp)(_ && _)
      prop.run(max, n, rng)
  }

  def forAllPow2Banded[A](sg: SGen[A])(pred: A => Boolean): Prop =
    forAllPow2Banded(sg(_))(pred)

  def forAllPow2Banded[A](f: Int => Gen[A])(pred: A => Boolean): Prop =
    forAll(n =>
      n match {
        case n if n == 0 => f(0)
        case n => {
          val m = math.pow(2, n - 1).toInt
          Gen.choose(m, 2 * m).flatMap(f)
        }
      }
    )(pred)

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified(List(), List(), 0)
  }

  /** Primary user interface to launch testing.
    *
    * @param p Property (Prop) to be tested.
    * @param maxsize Maximum "size" to be tested.  Here size could mean
    *                the size of the data structures or values within, depends
    *                how the Prop is defined.
    * @param testCases Number of testcases that need to be be conducted until
    *                  the Prop is considered "passed."
    * @param rng The source of randomness.  Pass a definite value for
    *            reproducible tests.
    * @note Test cases start small and work their way larger.
    * @note Defaults to 10 tests per size.
    */
  def run(
      p: Prop,
      maxSize: Int = 100,
      testCases: Int = 1000,
      rng: RNG = LCG(System.currentTimeMillis)
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Proved =>
        println(s"+ OK, proved property.")
      case Passed =>
        println(s"+ OK, property passed ${testCases} test cases.")
      case Falsified(fL, oeL, n) => {
        println(s"! Falsified after ${n} successful testcases.")
        for ((f, oe) <- fL zip oeL) {
          println(s"! Failed case:")
          println(s"!   ${f}")
          oe map { e =>
            println(s"! generated exception:")
            println(s"!   ${e}")
            println(s"! with stack trace:")
            println(s"!   ${e.getStackTrace.mkString("\n!   ")}")
          }
        }
      }
    }

}

/** Represents a generator of test cases. */
case class Gen[+A](sample: Rand[A]) {

  import Gen._

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen { sample flatMap { a => f(a).sample } }

  def map[B](f: A => B): Gen[B] = Gen { sample map f }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen { sample.map2(g.sample)(f) }

  /** Lifted product of two generators. */
  def **[B](g: Gen[B]): Gen[(A, B)] = this.map2(g)((_, _))

  /** Generates lists of type List[A] with random length */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { n =>
      Gen { Rand.sequence(List.fill(n)(sample)) }
    }

  /** Generate lists of type List[A] with a definite length */
  def listOfN(size: Int): Gen[List[A]] = listOfN(unit(size))

  /** SGen to produce lists Gens of List[A] of a definite length */
  def listOf: SGen[List[A]] = SGen { n => listOfN(n) }

  /** Like listOf, but don't produce empty lists */
  def listOf1: SGen[List[A]] = SGen { n => listOfN(n.max(1)) }

  def indexedSeqOfN(size: Gen[Int]): Gen[IndexedSeq[A]] =
    size flatMap { n =>
      Gen { Rand.sequenceIndexedSeq(IndexedSeq.fill(n)(sample)) }
    }

  def indexedSeqOfN(size: Int): Gen[IndexedSeq[A]] = indexedSeqOfN(unit(size))

  def indexedSeqOf: SGen[IndexedSeq[A]] = SGen { n => indexedSeqOfN(n) }

  def indexedSeqOf1: SGen[IndexedSeq[A]] =
    SGen { n => indexedSeqOfN(n.max(1)) }

  /** Convenience function to convert to an SGen.
    *
    * Handles some edge cases the implicit def does
    * not work without type annotation hints.
    */
  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  /** Allows pattern matching of the unlifted product of two generators.
    *   @note Simply matches just a tuple.
    */
  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  /** A "lazy" unit which always generates the same value. */
  def unit[A](a: => A): Gen[A] = Gen(Rand.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Rand.exclusiveIntRange(start, stopExclusive))

  def boolean: Gen[Boolean] = Gen(Rand.boolean)

  def rng: Gen[RNG] = Gen(Rand.rng)

  /** Pull from 2 Generators of the same type with equal likelihood */
  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] =
    Gen { Rand.joint2(0.5)(gen1.sample, gen2.sample) }

  /** Pull from 2 Generators with relative weights */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val prob1 = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen { Rand.joint2(prob1)(g1._1.sample, g2._1.sample) }
  }

  // Helper functions for Prop.forAll
  def sampleStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng) { rng1 =>
      Some(g.sample.action.run(rng1))
    }

  // Some convenience functions
  def listOf[A](g: Gen[A]): SGen[List[A]] = g.listOf
  def listOf1[A](g: Gen[A]): SGen[List[A]] = g.listOf1
  def indexedSeqOf[A](g: Gen[A]): SGen[IndexedSeq[A]] = g.indexedSeqOf
  def indexedSeqOf1[A](g: Gen[A]): SGen[IndexedSeq[A]] = g.indexedSeqOf1

}

/** Generates Gens of given sizes. */
case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen { n =>
      forSize(n) flatMap { a => f(a).forSize(n) }
    }

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  def map2[B, C](sg: SGen[B])(f: (A, B) => C): SGen[C] = SGen { n =>
    forSize(n).map2(sg.forSize(n))(f)
  }

  /** Lifted product of two generators. */
  def **[B](sg: SGen[B]): SGen[(A, B)] = this.map2(sg)((_, _))

}
