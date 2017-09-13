package fpinscala.chap07.parallelism

import java.util.concurrent.Executors

import scala.util.{Try, Success, Failure}

import fpinscala.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Par */
object ParParFutures2 {

  def main(args: Array[String]): Unit = {

    val numThreads = if (args.isEmpty) 5 else args(0).toInt
    
    val es = Executors.newFixedThreadPool(numThreads)

    // Simple test map2ViaActors vs map2ViaFlatMap

    println("\nSimple map2ViaActor vs map2ViaFlatMap comparison:")
    val map2_viaActor_42 = unit(22).map2ViaActor(unit(20))(_ + _)
    val map2_viaFlatMap_42 = unit(22).map2ViaFlatMap(unit(20))(_ + _)

    println(s"map2_viaActor_42 -> ${map2_viaActor_42.run(es)}")
    println(s"map2_viaFlatMap_42 -> ${map2_viaFlatMap_42.run(es)}")

    // Contrived test map2ViaActors(default) vs map2ViaFlatMap
    //
    // Not much difference timewise, both fast and can be
    // done with a threadpool of 1.
    // 
    // Program stackover flows for Lists much bigger than 2000
    //
    // When I put the two run methods back-to-back, I noticed
    // that program gets stack cranky for threadpools greater
    // than 8.

    println("\nContrived map2ViaActor vs map2ViaFlatMap comparison:")
    val list1to1000 = (1 to 1000).toList

    val viaMap2 = list1to1000.map(unit(_) map ((ii: Int) => 
      ii % 42)).foldLeft(unit(0))((p1: Par[Int], p2: Par[Int]) => 
        p1.map2(p2)(_ + _)) map { println(_: Int) }

    viaMap2.run(es)

    val viaFlatMap = list1to1000.map(unit(_) mapViaFlatMap ((ii: Int) => 
      ii % 42)).foldLeft(unit(0))((p1: Par[Int], p2: Par[Int]) => 
        p1.map2ViaFlatMap(p2)(_ + _)) map { println(_: Int) }

    viaFlatMap.run(es)

    // Stack crankier if instead I put these back-to-back
    //   foo.run(es)
    //   bar.run(es)
    //
    // but not if I instead do
    //   val foobar = (foo.run(es), bar.run(es))
    //   print(foobar)
    //
    // nor if I instead do
    //   print((foo.run(es), bar.run(es)))

    // Compare flatMap implementations

    val makeSlowBoolPar = (bool: Boolean) => lazyUnit { 
      Thread.sleep(2000)
      bool
    }

    val makeSlowAnsPar = (ans: String) => lazyUnit { 
      Thread.sleep(2000)
      ans
    }

    println("\nCompare flatMap implementations (~4 seconds each):")

    val withoutJoin = choice(makeSlowBoolPar(true))(
        makeSlowAnsPar("True choice")
      , makeSlowAnsPar("False choice"))

    println(s"without join -> ${withoutJoin.run(es)}")

    val withJoin = choiceViaJoin(makeSlowBoolPar(false))(
        makeSlowAnsPar("True choice")
      , makeSlowAnsPar("False choice"))

    println(s"with join -> ${withJoin.run(es)}")

    println("\nCompare join implementations (~4 seconds each):")

    val ppb1: Par[Par[Boolean]] = makeSlowBoolPar(true) map makeSlowBoolPar
    val ppb2: Par[Par[Boolean]] = makeSlowBoolPar(true) map makeSlowBoolPar
    val ppb3: Par[Par[Boolean]] = makeSlowBoolPar(true) map makeSlowBoolPar

    println(s"join via outside -> ${joinBlockingOutside(ppb2).run(es)}")
    println(s"join via inside -> ${joinBlockingInside(ppb3).run(es)}")
    println(s"join via flatMap -> ${joinViaFlatMap(ppb1).run(es)}")

    println()

    es.shutdown

  }

}
