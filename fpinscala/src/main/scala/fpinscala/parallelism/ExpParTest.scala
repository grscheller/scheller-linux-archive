package fpinscala.chap07.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par
import Par._

import scala.collection.immutable.Stream.{cons, from}

/** Implement an exponential Taylor series exapansion,
 *  using fpinscala.parallelism.Par.
 *
 *  Actually, this calculation I concocted only uses
 *  one thread from the thread pool over and over again.
 *
 *  By slipping in an extra .map(fork(_)) into the calculation
 *  after the .map(en(_)), I see that I need a dedicated
 *  thread for each fork.  This presents some deadlocking
 *  problems with our API (pre section 7.4.4).
 *
 */
object ExpParTest {

  // Construct a parallel exponential calculation
  def expPar(x: Double, maxTerm: Int): Par[Double] = {

    // nth term of the exponential Taylor series about 0
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
      xs.take(n).foldLeft(1.0) {_ * _}
    }

    // Use a stream to construct the parallel calculation
    //   Using foldRight sums smallest numbers in the series
    //   first.  Minimizes round off error.  
    from(0).take(maxTerm + 1)
           .map(en(_))
           .foldRight(unit(0.0))((term, sum) => map2(term, sum)(_ + _))
  }

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(1)

    print("\nrun(es)(expPar(1.0, 20)).get = ")
    println(run(es)(expPar(1.0, 20)).get)
    print("scala.math.exp(1.0) = ")
    println(scala.math.exp(1.0))

    print("\nrun(es)(expPar(10.0, 20)).get = ")
    println(run(es)(expPar(10.0, 20)).get)
    print("run(es)(expPar(10.0, 172)).get = ")
    println(run(es)(expPar(10.0, 172)).get)
    print("scala.math.exp(10.0) = ")
    println(scala.math.exp(10.0))

    es.shutdown

    println()

  }
}
