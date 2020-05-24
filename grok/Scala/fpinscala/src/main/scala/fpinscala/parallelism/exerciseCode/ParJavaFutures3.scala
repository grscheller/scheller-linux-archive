package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.javaFutures.Par
import Par._

/** Test fpinscala.parallelism.Blocking object
 *
 *  Post Exercise 7.6
 *
 */
object ParJavaFutures3 {

  // Model an expensive calculations with
  // a less than stellar implementations
  // of the factorial functon.
  def fib(n: Long): Long =
    if (n < 2) n else fib(n-1) + fib(n-2)

  // Non-parallel versions of functions
  // which I wish to implement in parallel.

  def sumDoubles(doubles: IndexedSeq[Double]): Double = {
    val size = doubles.size
    size match {
      case 0|1 => doubles.headOption getOrElse 0.0
      case   _ => { val (l,r) = doubles.splitAt(size/2)
                    sumDoubles(l) + sumDoubles(r) }
    }
  }

  def maxInts(ints: IndexedSeq[Int]): Int = {
    val size = ints.size
    size match {
      case 0 => throw new IllegalArgumentException(
                  "IndexedSeq must be non-empty")
      case 1 => ints(0)
      case _ => { val (l,r) = ints.splitAt(size/2)
                  val maxL = maxInts(l)
                  val maxR = maxInts(r)
                  if (maxL < maxR) maxR else maxL }
    }
  }

  // Parallel versions of above two functions

  /** Parallel calculation to sum an IndexedSeq[Double] */
  def sumDoublesParallel(xs: IndexedSeq[Double]): Par[Double] =
    if (xs.isEmpty)
      unit(0.0)
    else
      balancedBinComp(xs.map(unit(_)))(_ + _)

  /** Parallel calculation to sum an IndexedSeq[Double] */
  def maxIntsParallel(xs: IndexedSeq[Int]): Par[Int] =
    if (xs.isEmpty)
      throw new IllegalArgumentException("Max of an empty collection")
    else
      balancedBinComp(xs.map(unit(_))) { (l, r) => if (l < r) r else l }

  def main(args: Array[String]): Unit = {

    // val es = Executors.newFixedThreadPool(100)
    val es = Executors.newCachedThreadPool()

    val fibParameter = 43L

    // Test map3, map4, and map5
    println("\nTest map3, map4, and map5:")

    val fooPar0 = lazyUnit(fib(fibParameter - 1))
    val fooPar1 = lazyUnit(fib(fibParameter))
    val fooPar2 = lazyUnit(fib(fibParameter + 1))
    val barPar1 = lazyUnit(fib(fibParameter))
    val barPar2 = lazyUnit(fib(fibParameter + 1))
    val barPar3 = unit(0.0)
    val combine4 = ( a: Double, b: Double
                     , c: Double, d: Double ) => (a + b - c + d)/4.0

    val foobar5Par = fooPar0.map5(
        fooPar1
      , fooPar2
      , barPar1
      , barPar2)((_,_,_,_,_))
    val foobar5Fut = foobar5Par.future(es)

    print("\nfoobar5Fut.get() = "); println(foobar5Fut.get)

    val foobar3Par = fooPar0.map3(fooPar1, fooPar2)((_,_,_))
    val foobar3Fut1 = foobar3Par.future(es)
    val foobar3Fut2 = foobar3Par.future(es)
    print("foobar3Fut1.get() = "); println(foobar3Fut1.get)
    print("foobar3Fut2.get() = "); println(foobar3Fut2.get)

    val foobar4Par = fooPar0.map(_.toDouble).map4(
        fooPar1.map(_.toDouble)
      , fooPar2.map(_.toDouble)
      , barPar3 )(combine4)
    val foobar4Fut = foobar4Par.future(es)
    print("foobar4Fut.get() = "); println(foobar4Fut.get)

    // Simple tests for the parallel and non-parallel
    // methods to be compared.
    println("\nSimple tests:")
 
    val vecInt = Vector(1, 2, 3, 0, 5, 6, 7, 42, 9, 10)
    val vecDouble = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).map(_.toDouble)

    print("\nvecDouble = "); println(vecDouble)

    print("sumDoubles(vecDouble) = "); println(sumDoubles(vecDouble))
    print("sumDoublesParallel(vecDouble).run(es) = ")
    println(sumDoublesParallel(vecDouble).run(es))

    print("\nvecInt = "); println(vecInt)
    print("maxInts(vecInt) = "); println(maxInts(vecInt))
    print("maxIntsParallel(vecInt).run(es) = ")
    println(maxIntsParallel(vecInt).run(es))

    // More numerically instensive tests for the parallel
    // and non-parallel methods to be compared.
    println("\nNumerically intensive tests:\n")

    val bigList = List.range[Long](fibParameter, 0, -1)
    val smallList = List.range[Long](fibParameter+2, fibParameter-3, -1)

    println {
      print("smallList map fib = \n  ")
      val t0 = System.nanoTime
      val hold = smallList map fib
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("parMap(smallList)(fib).run(es) = \n  ")
      val t0 = System.nanoTime
      val hold = parMap(smallList)(fib).run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }

    println {
      print("\nbigList map fib = \n  ")
      val t0 = System.nanoTime
      val hold = bigList map fib
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("parMap(bigList)(fib).run(es) = \n  ")
      val t0 = System.nanoTime
      val hold = parMap(bigList)(fib).run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    val foo: Par[List[Long]] =
      sequence(bigList map asyncF(fib)).map {
        a => a.filter(_ % 2 == 0)
      }

    val bar: Par[List[Long]] =
      parMap(bigList)(fib) map {
        a => a.filter(_ % 2 == 0)
      }

    println {
      print("\nmap with asyncF  = ")
      val t0 = System.nanoTime
      val hold = foo.run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("calc with parMap = ")
      val t0 = System.nanoTime
      val hold = bar.run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    val baz = parFilter(bigList)(a => fib(a)%2 == 0)

    println {
      print("\ncalc with parFilter = ")
      val t0 = System.nanoTime
      val hold = baz.run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("without parallelism  = ")
      val t0 = System.nanoTime
      val hold = bigList filter { a => fib(a)%2 == 0 }
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }

    val boz = parFilter(smallList)(a => fib(a)%2 == 0)

    println {
      print("\ncalc with parFilter = ")
      val t0 = System.nanoTime
      val hold = boz.run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("without parallelism  = ")
      val t0 = System.nanoTime
      val hold = smallList filter { a => fib(a)%2 == 0 }
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }

    es.shutdown

    println()

  }

}
