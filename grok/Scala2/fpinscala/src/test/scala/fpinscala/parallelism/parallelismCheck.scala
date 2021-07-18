package fpinscala.chap07.test.parallelism

import java.util.concurrent.{ExecutorService => ES}
import java.util.concurrent.Executors
import fpinscala.parallelism.{Par, ParProp}
import Par._

import fpinscala.testing.{Gen, SGen, Prop}
import fpinscala.state.rand.{Rand, RNG, LCG}

object parallelismCheck {

  def main(args: Array[String]): Unit = {

    // Start off with a simple straightforward implementation.
    println("\nTest Par.unit(1).map(_ + 1) equals Par.unit(2) law.")

    val es1 = Executors.newFixedThreadPool(4)

    val parLawSimple = Prop.check {
      val p1 = Par.unit(1) map { _ + 1 }
      val p2 = Par.unit(2)
      p1.run(es1) == p2.run(es1)
    }

    Prop.run(parLawSimple)

    // Lift into the Par Monad.
    println("\nRepeat test of law by lifting into Par monad.")
    def parEqual[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
      p1.map2(p2) { _ == _ }

    val parLawHandLifted = Prop.check {
      parEqual(Par.unit(1) map { _ + 1 }, Par.unit(2)) run es1
    }

    Prop.run(parLawHandLifted)

    // Lift into the Par Monad after putting in fpinscala.parallel package.
    println("\nRepeat again after updating Par companion object.")

    val parLawLifted = Prop.check {
      Par.equal(Par.unit(1) map { _ + 1 }, Par.unit(2)) run es1
    }

    Prop.run(parLawLifted)

    es1.shutdown

    // Create the threadpools
    val maxThreadNum = 8
    val threadPools: IndexedSeq[ES] =
      Range(0, maxThreadNum + 1) map { ii =>
        if (ii != 0)
          Executors.newFixedThreadPool(ii)
        else
          Executors.newCachedThreadPool
      }

    // 75% of time give one of the fixed number threadpools
    // 25% of time give the unbounded one.
    val esPool = Gen.weighted(
      (Gen.choose(1, maxThreadNum + 1) map threadPools, .75),
      (Gen.unit(threadPools(0)), .25)
    )

    // Test a true and a falsifiable proposition.
    val fooTrue =
      ParProp.forAllPar(Gen.choose(10, 20), esPool)(Par.asyncF(_ < 42))
    val fooFalse =
      ParProp.forAllPar(Gen.choose(1, 43), esPool)(Par.asyncF(_ < 42))

    // Run the tests.
    println("\n\nTest ParProp.forAllPar with something true.")
    Prop.run(fooTrue)
    println("\nTest ParProp.forAllPar with something falsifiable.")
    Prop.run(fooFalse)

    // Shut the threadpools down.
    threadPools map { _.shutdown }

    println()

  }

}
