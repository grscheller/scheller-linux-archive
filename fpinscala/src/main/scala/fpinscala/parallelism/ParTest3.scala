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

  def sum1(ints: IndexedSeq[Int]): Int = {
    val size = ints.size
    size match {
      case 0|1 => ints.headOption getOrElse 0
      case   _ => { val (l,r) = ints.splitAt(size/2)
                    sum1(l) + sum1(r) }
    }
  }

  def max1(ints: IndexedSeq[Int]): Int = {
    val size = ints.size
    size match {
      case 1 => ints(0)
      case 2 => if (ints(0) < ints(1)) ints(1) else ints(0)
      case _ => { val (l,r) = ints.splitAt(size/2)
                  val maxL = max1(l)
                  val maxR = max1(r)
                  if (maxL < maxR) maxR else maxL }
    }
  }

  def main(args: Array[String]): Unit = {

    // Test single threaded methods.
    val vec = Vector(2, 4, 3, 20, 0, 42, 5, 7, 42, 11, 13)
    print("\nvec = "); println(vec)
    print("sum1(vec) = "); println(sum1(vec))
    print("max1(vec) = "); println(max1(vec))
    
    val es = Executors.newFixedThreadPool(6)

    val fibParameter1 = 43L
    val fibParameter2 = 46L

    // Test map3, map4, and map5
    val fooPar0 = lazyUnit(fib(fibParameter1 - 1))
    val fooPar1 = lazyUnit(fib(fibParameter1))
    val fooPar2 = lazyUnit(fib(fibParameter1 + 1))
    val barPar1 = lazyUnit(fib(fibParameter1))
    val barPar2 = lazyUnit(fib(fibParameter1 + 1))
    val tuple3It = (a: Long, b: Long, c: Long) => (a, b, c)
    val tuple5It = (a: Long, b: Long, c: Long, d: Long, e: Long) =>
      (a, b, c, d, e)

    val foobar5Par = map5( fooPar0
                         , fooPar1
                         , fooPar2
                         , barPar1
                         , barPar2 )(tuple5It)
    val foobar5Fut = run(es)(foobar5Par)
    print("foobar5Fut.get() = "); println(foobar5Fut.get)

    val foobar3Par = map3( fooPar0
                         , fooPar1
                         , fooPar2 )(tuple3It)
    val foobar3Fut1 = run(es)(foobar3Par)
    val foobar3Fut2 = run(es)(foobar3Par)
    print("foobar3Fut1.get() = "); println(foobar3Fut1.get)
    print("foobar3Fut2.get() = "); println(foobar3Fut2.get)

    es.shutdown

    println()

  }

}
