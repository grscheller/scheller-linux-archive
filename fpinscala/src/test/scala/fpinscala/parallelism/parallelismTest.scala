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

    val pLaw = Prop.check {
      val p1 = Par.unit(1) map {_ + 1}
      val p2 = Par.unit(2)
      p1.run(es1) == p2.run(es1)
    }

    Prop.run(pLaw)

    es1.shutdown

    println()

  }

}
