package fpinscala.chap07.test.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import fpinscala.parallelism.Par
import Par._

import fpinscala.testing.{Gen,SGen,Prop}
import fpinscala.state.rand.{Rand,RNG,LCG}

object parallelismCheck {

  val rng0: RNG = LCG(System.currentTimeMillis)
  val rng1: RNG = LCG(3141592653589793L)
  val rng2: RNG = LCG(2718281828459045L)
  val rng3: RNG = LCG(1234567890L)

  /** Parse input args to determine scale of datastructure */
  def parseArgs(args: Array[String]): Long =
    if (args.length == 0) 100
    else args(0).toLong

  def main(args: Array[String]): Unit = {

    // Start off with a simple straightforward implementation.
    println("\nTest Par.unit(1).map(_ + 1) equals Par.unit(2) law.")

    val es1 = Executors.newFixedThreadPool(4)

    val parLawSimple = Prop.check {
      val p1 = Par.unit(1) map {_ + 1}
      val p2 = Par.unit(2)
      p1.run(es1) == p2.run(es1)
    }

    Prop.run(parLawSimple)

    // Lift into the Par Monad.
    println("\nRepeat test of law by lifting into Par monad.")
    def parEqual[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
      p1.map2(p2) { _ == _ }

    val parLawHandLifted = Prop.check {
      parEqual( Par.unit(1) map { _ + 1 }
              , Par.unit(2) ) run es1
    }

    Prop.run(parLawHandLifted)

    // Lift into the Par Monad after putting in fpinscala.parallel package.
    println("\nRepeat again after updating Par companion object.")

    val parLawLifted = Prop.check {
      Par.equal( Par.unit(1) map { _ + 1 }
               , Par.unit(2) ) run es1
    }

    Prop.run(parLawLifted)

    es1.shutdown

    println()

  }

}
