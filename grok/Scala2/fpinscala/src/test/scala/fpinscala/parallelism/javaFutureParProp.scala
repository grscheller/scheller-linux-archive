package fpinscala.chap07.test.parallelism

import java.util.concurrent.{ExecutorService => ES}
import java.util.concurrent.Executors
import fpinscala.parallelism.javaFutures.{Par, ParProp}
import Par._

import fpinscala.testing.{Gen, SGen, Prop}
import fpinscala.state.rand.{Rand, RNG, LCG}

object javaFutureParProp {

  def main(args: Array[String]): Unit = {

    // Create the threadpools
    val minThreadNum = 180
    val maxThreadNum = 190
    val threadPools: IndexedSeq[ES] =
      Range(minThreadNum - 1, maxThreadNum + 1) map { ii =>
        if (ii != minThreadNum - 1)
          Executors.newFixedThreadPool(ii)
        else
          Executors.newCachedThreadPool
      }

    // 50% of time give one of the fixed number threadpools
    // 50% of time give the unbounded one.
    val esPool = Gen.weighted(
      (Gen.choose(1, maxThreadNum - minThreadNum + 1) map threadPools, .5),
      (Gen.unit(threadPools(0)), .5)
    )

    def nap(n: Int): Double = {
      Thread.sleep(n)
      n / 1000.0
    }

    def sumSleepsLT(t: Double)(as: List[Int]): Par[Boolean] =
      Par.parMap(as)(nap) map { _.sum } map { _ < t }

    val listOf1SGen = Gen.choose(1, 11).listOf1

    def parallelSleep(sleepTime: Double) =
      ParProp.forAllPar(listOf1SGen, esPool)(sumSleepsLT(sleepTime)(_))

    // Run some tests.
    println("\nTest ParProp.forAllPar with an SGen - should usually succeed:")
    Prop.run(parallelSleep(5.0))
    println("\nTest ParProp.forAllPar with an SGen - should usually fail:")
    Prop.run(parallelSleep(0.1))
    println("\nTest ParProp.forAllPar with an SGen - more hit or miss:")
    Prop.run(parallelSleep(0.5))

    // Shut down the threadpools.
    threadPools map { _.shutdown }

    println()

  }

}
