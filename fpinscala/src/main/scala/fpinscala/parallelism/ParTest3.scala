package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par
import Par._

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
      case 1 => ints(0)
      case 2 => if (ints(0) < ints(1)) ints(1) else ints(0)
      case _ => { val (l,r) = ints.splitAt(size/2)
                  val maxL = maxInts(l)
                  val maxR = maxInts(r)
                  if (maxL < maxR) maxR else maxL }
    }
  }

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(10)

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

    es.shutdown

    println()

  }

}
