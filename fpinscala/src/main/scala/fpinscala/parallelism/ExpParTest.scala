package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par
import Par._

import scala.collection.immutable.Stream.{cons, from}

/** Test fpinscala.parallelism.Par object
 *
 *
 */
object ExpParTest {

  /** Construct a parallel exponential calculation */
  def expPar(x: Double, maxTerm: Int): Par[Double] = {

    def en(n: Int): Par[Double] =
      n match {
        case  0 =>  unit(1.0)
        case  1 =>  unit(x)
        case nn =>  lazyUnit(pow(x, nn)/fact(nn))
      }

    def fact(m: Int): Double = {
      def loop(accum: Double, mm: Int): Double = 
        mm match {
          case 0   => accum
          case mm  => loop(mm*accum, mm - 1)
        }
      loop(1.0, m)
    }

    def pow(x: Double, n: Int): Double = {
      lazy val xs: Stream[Double] = cons(x, xs)
      xs.take(n).foldRight(1.0) {_ * _}
    }

    // Use a stream to construct a parallel calculation
    from(0).take(maxTerm + 1)
           .map(en(_))
           .foldRight(unit(0.0))((term, sum) =>
              map2(term, sum)(_ + _))

  }

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(4)

    print("\nrun(es)(expPar(1.0, 30)).get = ")
    println(run(es)(expPar(1.0, 30)).get)

    es.shutdown

    print("scala.math.exp(1.0) = ")
    println(scala.math.exp(1.0))

    println()

  }
}
