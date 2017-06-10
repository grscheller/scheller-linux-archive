package fpinscala.chap07.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeoutException

import scala.util.{Try, Success, Failure}

import fpinscala.parallelism.Par
import Par._

/** Test fpinscala.parallelism.Par */
object ParParFutures2 {

  def main(args: Array[String]): Unit = {

    val numThreads = if (args.isEmpty) 5 else args(0).toInt
    
    val es = Executors.newFixedThreadPool(numThreads)

    // Simple test map2_viaActors vs map2_viaFlatMap

    println("\nSimple map2_viaActor vs map2_viaFlatMap comparison:")
    val map2_viaActor_42 = unit(22).map2_viaActor(unit(20))(_ + _)
    val map2_viaFlatMap_42 = unit(22).map2_viaFlatMap(unit(20))(_ + _)

    println(s"map2_viaActor_42 -> ${map2_viaActor_42.run(es)}")
    println(s"map2_viaFlatMap_42 -> ${map2_viaFlatMap_42.run(es)}")

    // Contrived test map2_viaActors(default) vs map2_viaFlatMap
    //
    // Not much difference timewise, both fast and can be
    // done with a threadpool of 1.
    // 
    // Program stackover flows for Lists much bigger than 2000
    //
    // When I put the two run methods back-to-back, I noticed
    // that program gets stack cranky for threadpools greater
    // than 8.

    println("\nContrived map2_viaActor vs map2_viaFlatMap comparison:")
    val list1to2000 = (1 to 2000).toList

    val foo = list1to2000.map(unit(_) map ((ii: Int) => 
      ii % 42)).foldLeft(unit(0))((p1: Par[Int], p2: Par[Int]) => 
        p1.map2(p2)(_ + _)) map { println(_: Int) }

    foo.run(es)

    val bar = list1to2000.map(unit(_) map_viaFlatMap ((ii: Int) => 
      ii % 42)).foldLeft(unit(0))((p1: Par[Int], p2: Par[Int]) => 
        p1.map2_viaFlatMap(p2)(_ + _)) map { println(_: Int) }

    bar.run(es)

    // Stack crankier if instead I put these back-to-back
    //   foo.run(es)
    //   bar.run(es)

    // but not if I instead do
    //   val foobar = (foo.run(es), bar.run(es))
    //   print(foobar)

    // nor if I instead do
    //   print((foo.run(es), bar.run(es)))

    println()

    es.shutdown

  }

}
