package fpinscala.chap07.parallelism

import java.util.concurrent._
//import fpinscala.parallelism.javaFutures.Par
import fpinscala.parallelism.Par
import Par._

import scala.collection.immutable.LazyList.{cons, from}

/** Compare different implementations for exponential Taylor series exapansion.
 *
 *  Using fpinscala.parallelism.Par.
 *
 *  Sum smallest numbers in the series
 *  first to minimizes round off error.  
 *
 */
object ExpParTest {

  /** Construct a parallel exponential calculation via streams.
   *  
   *    Done before exercise 7.4
   */
  def expParStream(x: Double, maxTerm: Int): Par[Double] = {

    lazy val xs: LazyList[Double] = cons(x, xs)

    /** Par for nth term of the Taylor series about 0 */
    def enPar(n: Int): Par[Double] =
      n match {
        case  0 =>  unit(1.0)
        case  1 =>  unit(x)
        case nn =>  lazyUnit(powx(nn)/fact(nn))
      }

    def powx(n: Int): Double = xs.take(n).foldLeft(1.0) {_ * _}

    from(0).take(maxTerm + 1)
           .map(enPar(_))
           .foldRight(unit(0.0))((term, sum) => term.map2(sum)(_ + _))
  }

  /** Construct a parallel exponential calculation via parMap.
   *  
   *    Done after exercise 7.6
   */
  def expParMap(x: Double, maxTerm: Int): Par[Double] = {

    /** nth term of the Taylor series about 0 */
    def en(n: Int): Double = powx(n)/fact(n)

    def powx(n: Int): Double = {
      @annotation.tailrec
      def loop(accum: Double, nn: Int): Double = 
        nn match {
          case 0  => accum
          case nn => loop(x*accum, nn - 1)
        }
      loop(1.0, n)
    }

    val countDown = List.iterate(maxTerm, maxTerm + 1)(_ - 1)
    val parList = parMap(countDown)(en(_))
    parList map { _.foldLeft(0.0)(_ + _) }

  }

  /** Exponential function.
   *  
   *   Done without Pars.
   *
   */
  def expFun(x: Double, maxTerm: Int): Double = {

    /** nth term of the Taylor series about 0 */
    def en(n: Int): Double = powx(n)/fact(n)

    def powx(n: Int): Double = {
      @annotation.tailrec
      def loop(accum: Double, nn: Int): Double = 
        nn match {
          case 0  => accum
          case nn => loop(x*accum, nn - 1)
        }
      loop(1.0, n)
    }

    val countDown = List.iterate(maxTerm, maxTerm + 1)(_ - 1)
    countDown.map(en _).foldLeft(0.0)(_ + _)

  }

  //* Done imperitively to see if I can beat the parallelism. */
  def expLoopFun(x: Double, maxTerm: Int): Double = {

    import scala.math.pow

    var jj = maxTerm
    var factjj = fact(maxTerm)
    var accum = 0.0
    while (jj > 1) {
      accum = accum + pow(x, jj)/factjj
      factjj = factjj/jj
      jj = jj - 1
    }
    accum + 1.0 + x

  }

  //* Compute factorial - no idiot checking. */
  def fact(m: Int): Double = {
    @annotation.tailrec
    def loop(accum: Double, mm: Int): Double = 
      mm match {
        case 0   => accum
        case mm  => loop(mm*accum, mm - 1)
      }
    loop(1.0, m)
  }

  /** Parse input args to determine number of threads in ES */
  def parseArgs(args: Array[String]): Int =
    if (args.length == 0) 35
    else args(0).toInt

  def main(args: Array[String]): Unit = {

    val es = Executors.newFixedThreadPool(parseArgs(args))

    println()

    println {
      print("scala.math.exp(1.0) = ")
      val t0 = System.nanoTime
      val hold = scala.math.exp(1.0)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }
    println {
      print("expParStream(1.0, 20).run(es) = ")
      val t0 = System.nanoTime
      val hold = expParStream(1.0, 20).run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }
    println {
      print("expParMap(1.0, 20).run(es) = ")
      val t0 = System.nanoTime
      val hold = expParMap(1.0, 20).run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }
    println {
      print("expFun(1.0, 20) = ")
      val t0 = System.nanoTime
      val hold = expFun(1.0, 20)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }
    println {
      print("expLoopFun(1.0, 20) = ")
      val t0 = System.nanoTime
      val hold = expLoopFun(1.0, 20)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println()

    println {
      print("scala.math.exp(0.42) = ")
      val t0 = System.nanoTime
      val hold = scala.math.exp(0.42)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }
    println {
      print("expParStream(0.42, 20).run(es) = ")
      val t0 = System.nanoTime
      val hold = expParStream(0.42, 20).run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }
    println {
      print("expParMap(0.42, 20).run(es) = ")
      val t0 = System.nanoTime
      val hold = expParMap(0.42, 20).run(es)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }
    println {
      print("expFun(0.42, 20) = ")
      val t0 = System.nanoTime
      val hold = expFun(0.42, 172)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }
    println {
      print("expLoopFun(0.42, 20) = ")
      val t0 = System.nanoTime
      val hold = expLoopFun(0.42, 20)
      val t1 = System.nanoTime
      hold.toString + " in " + (t1 - t0)/1000.0 + " μs."
    }

    println()

    es.shutdown

    println()

  }
}
