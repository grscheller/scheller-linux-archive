package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par._

/** Test fpinscala.parallelism.Par object
 *
 *  Post Exercise 7.6
 *
 */
object ParTest3 {

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

    val es = Executors.newFixedThreadPool(100)

    val fibParameter = 43L

    // Test map3, map4, and map5
    println("\nTest map3, map4, and map5:")

    val fooPar0 = lazyUnit(fib(fibParameter - 1))
    val fooPar1 = lazyUnit(fib(fibParameter))
    val fooPar2 = lazyUnit(fib(fibParameter + 1))
    val barPar1 = lazyUnit(fib(fibParameter))
    val barPar2 = lazyUnit(fib(fibParameter + 1))
    val barPar3 = unit(0.0)
    val tuple3It = (a: Long, b: Long, c: Long) => (a, b, c)
    val tuple5It = (a: Long, b: Long, c: Long, d: Long, e: Long) =>
      (a, b, c, d, e)
    val combine4 = ( a: Double, b: Double
                     , c: Double, d: Double ) => (a + b - c + d)/4.0

    val foobar5Par = map5( fooPar0
                         , fooPar1
                         , fooPar2
                         , barPar1
                         , barPar2 )(tuple5It)
    val foobar5Fut = run(es)(foobar5Par)
    print("\nfoobar5Fut.get() = "); println(foobar5Fut.get)

    val foobar3Par = map3( fooPar0
                         , fooPar1
                         , fooPar2 )(tuple3It)
    val foobar3Fut1 = run(es)(foobar3Par)
    val foobar3Fut2 = run(es)(foobar3Par)
    print("foobar3Fut1.get() = "); println(foobar3Fut1.get)
    print("foobar3Fut2.get() = "); println(foobar3Fut2.get)

    val foobar4Par = map4( map(fooPar0)(_.toDouble)
                         , map(fooPar1)(_.toDouble)
                         , map(fooPar2)(_.toDouble)
                         , barPar3 )(combine4)
    val foobar4Fut = run(es)(foobar4Par)
    print("foobar4Fut.get() = "); println(foobar4Fut.get)

    // Simple tests for the parallel and non-parallel
    // methods to be compared.
    println("\nSimple tests:")
 
    val vecInt = Vector(1, 2, 3, 0, 5, 6, 7, 42, 9, 10)
    val vecDouble = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).map(_.toDouble)

    print("\nvecDouble = "); println(vecDouble)

    print("sumDoubles(vecDouble) = "); println(sumDoubles(vecDouble))
    print("run(es)(sumDoublesParallel(vecDouble)).get = ")
    println(run(es)(sumDoublesParallel(vecDouble)).get)

    print("\nvecInt = "); println(vecInt)
    print("maxInts(vecInt) = "); println(maxInts(vecInt))
    print("run(es)(maxIntsParallel(vecInt)).get = ")
    println(run(es)(maxIntsParallel(vecInt)).get)

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
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("run(es)(parMap(smallList)(fib)).get = \n  ")
      val t0 = System.nanoTime
      val hold = run(es)(parMap(smallList)(fib)).get
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }

    println {
      print("\nbigList map fib = \n  ")
      val t0 = System.nanoTime
      val hold = bigList map fib
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("run(es)(parMap(bigList)(fib)).get = \n  ")
      val t0 = System.nanoTime
      val hold = run(es)(parMap(bigList)(fib)).get
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    val foo: Par[List[Long]] =
      map(sequence(bigList map asyncF(fib)))(a => a.filter(_ % 2 == 0))

    val bar: Par[List[Long]] =
      map(parMap(bigList)(fib))(a => a.filter(_ % 2 == 0))

    println {
      print("\nmap with asyncF  = ")
      val t0 = System.nanoTime
      val hold = run(es)(foo).get
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("calc with parMap = ")
      val t0 = System.nanoTime
      val hold = run(es)(bar).get
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    val baz1 = parFilter1(bigList)(a => fib(a)%2 == 0)
    val baz2 = parFilter2(bigList)(a => fib(a)%2 == 0)

    println {
      print("\ncalc with parFilter1 = ")
      val t0 = System.nanoTime
      val hold = run(es)(baz1).get
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("calc with parFilter2 = ")
      val t0 = System.nanoTime
      val hold = run(es)(baz2).get
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }

    println {
      print("without parallelism  = ")
      val t0 = System.nanoTime
      val hold = bigList filter { a => fib(a)%2 == 0 }
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }

    val boz1 = parFilter1(smallList)(a => fib(a)%2 == 0)
    val boz2 = parFilter2(smallList)(a => fib(a)%2 == 0)

    println {
      print("\ncalc with parFilter1 = ")
      val t0 = System.nanoTime
      val hold = run(es)(boz1).get
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }
 
    println {
      print("calc with parFilter2 = ")
      val t0 = System.nanoTime
      val hold = run(es)(boz2).get
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }

    println {
      print("without parallelism  = ")
      val t0 = System.nanoTime
      val hold = smallList filter { a => fib(a)%2 == 0 }
      val t1 = System.nanoTime
      hold + " in " + (t1 - t0)/1000000000.0 + " seconds\n"
    }

    es.shutdown

    println()

  }

}
