package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par
import Par._

import scala.collection.immutable.Stream.{cons, from}

/** Two implementations for exponential Taylor series exapansion,
 *
 *  Using fpinscala.parallelism.Par.
 *
 *  Both sum smallest numbers in the series
 *  first to minimizes round off error.  
 *
 */
object ExpParTest {

  /** Construct a parallel exponential calculation via streams.
   *  
   *    Done before exercise 7.4
   */
  def expParStream(x: Double, maxTerm: Int): Par[Double] = {

    /** Par for nth term of the Taylor series about 0 */
    def en(n: Int): Par[Double] =
      n match {
        case  0 =>  unit(1.0)
        case  1 =>  unit(x)
        case nn =>  lazyUnit(powx(nn)/fact(nn))
      }

    def fact(m: Int): Double = {
      def loop(accum: Double, mm: Int): Double = 
        mm match {
          case 0   => accum
          case mm  => loop(mm*accum, mm - 1)
        }
      loop(1.0, m)
    }

    def powx(n: Int): Double = {
      lazy val xs: Stream[Double] = cons(x, xs)
      xs.take(n).foldLeft(1.0) {_ * _}
    }

    from(0).take(maxTerm + 1)
           .map(en(_))
           .foldRight(unit(0.0))((term, sum) => map2(term, sum)(_ + _))
  }

  /** Construct a parallel exponential calculation via parMap.
   *  
   *    Done after exercise 7.6
   */
  def expParMap(x: Double, maxTerm: Int): Par[Double] = {

    /** nth term of the Taylor series about 0 */
    def en(n: Int): Double = powx(n)/fact(n)

    def fact(m: Int): Double = {
      def loop(accum: Double, mm: Int): Double = 
        mm match {
          case 0   => accum
          case mm  => loop(mm*accum, mm - 1)
        }
      loop(1.0, m)
    }

    def powx(n: Int): Double = {
      def loop(accum: Double, nn: Int): Double = 
        nn match {
          case 0  => accum
          case nn => loop(x*accum, nn - 1)
        }
      loop(1.0, n)
    }

    val countDown = List.iterate(maxTerm, maxTerm + 1)(_ - 1)
    val parList = parMap(countDown)(en(_))
    map(parList)(_.foldLeft(0.0)(_ + _))

  }

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(2)

    print("\nscala.math.exp(1.0) = ")
    println(scala.math.exp(1.0))
    print("run(es)(expParStream(1.0, 20)).get = ")
    println(run(es)(expParStream(1.0, 20)).get)
    print("run(es)(expParMap(1.0, 20)).get = ")
    println(run(es)(expParMap(1.0, 20)).get)

    println()

    print("scala.math.exp(10.0) = ")
    println(scala.math.exp(10.0))
    print("run(es)(expParStream(10.0, 20)).get = ")
    println(run(es)(expParStream(10.0, 20)).get)
    print("run(es)(expParMap(10.0, 20)).get = ")
    println(run(es)(expParMap(10.0, 20)).get)

    println()

    print("scala.math.exp(10.0) = ")
    println(scala.math.exp(10.0))
    print("run(es)(expParStream(10.0, 172)).get = ")
    println(run(es)(expParStream(10.0, 172)).get)
    print("run(es)(expParMap(10.0, 172)).get = ")
    println(run(es)(expParMap(10.0, 172)).get)

    es.shutdown

    println()

  }
}
