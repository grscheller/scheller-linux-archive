package fpinscala.parallelism

import java.util.concurrent.{Future,ExecutorService}
import java.util.concurrent.{Callable,TimeUnit}
import java.time.Duration
import java.time.Instant.now
//import scala.util.{Try, Success, Failure}

/** Par object.
 *
 */
object Par {

  type Par[A] = ExecutorService => Future[A]

  /** Par helper class to wrap a constant value into a Future.
   *
   *  Does not use the underlying es (ExecutorService),
   *  basically, born done.
   *
   */
  private final
  case class UnitFuture[A](get: A) extends Future[A] {

    def get(timeout: Long, units: TimeUnit): A = get

    def isDone = true

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = true

  }

  /** Wrap a constant value in a Par. 
   *
   *  Does not actually use the underlying java or OS
   *  multi-threading mechanisms.  The Par it returns
   *  is a constant function and does not depend on
   *  whatever (es: ExecutorService) is passed to it.  
   *
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /** Lazy version of unit
   *
   *  Evaluates its argument in a separate thread
   *  only if required.
   *
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Return a Future for a parallel calculation.
    *
    * The run method does not return the final value of
    * type A, but an imperitive java wee beasty called
    * a Future.  You will need to use its get method
    * to actually get the value of type A.
    *
    * The run method returns the Future right away.  The
    * Future's get method blocks until the value is available.
    *
    * So, run is functional code which returns a Future[A].
    * This could be useful if you had to pass it to existing
    * java code.
    *
    * In scala code, I would guess best practices would be
    * to either push these Futures to the "outside edge" of
    * the program or completely encapsulate them in a
    * functional interface.
    *
    */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /** Mark a calculation to be done in a parallel thread
   *  when the resulting Future is eventially evaluated.
   *
   *  In Java 8, Future now has a functional interface. 
   *
   *  Before java 8, I would have had to define fork like
   *  def fork[A](a: => Par[A]): Par[A] =
   *    es => es.submit(new Callable[A] { def call = a(es).get })
   *
   *  For backward compatibility, an instance of an interface
   *  containing a single abstract method (SAM) can be
   *  used anywhere a function object is expected.  So, above
   *  implementation of fork would still work.
   *
   */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(() => a(es).get)

  /** Combined two parallel computations with a function.
   *
   *  Function not evaluated in a separate thread.  To
   *  do that, use `fork(map2(a,b)(f))'
   *
   *  Also, a Par returns a Future, not necessarily a
   *  UnitFuture, so I am restricted to the Future API.
   *
   *  What I can do directly here to respect the "time out" 
   *  contract for get method of Future is limited
   *  since the Par API has no apriori mechanisms 
   *  for adding timeouts to the parallel calculation.
   *
   *  Looking at the book's answer key solution, the 
   *  trick is to wrap the futures I am passed into
   *  another future object.
   *
   *  Here is my starting point.
   */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      Map2Future(af, bf, f)
    }

  /** Par.map2 helper class.
   *
   *  Wrap the two futures into another future so
   *  that f can be evaluated after get is called.
   *
   */
  private final
  //case class Map2Future[A,B,C](af: Future[A],
  //                             bf: Future[B],
  //                              f: (A,B) => C) extends Future[C] {
  def Map2Future[A,B,C](af: Future[A],
                        bf: Future[B],
                         f: (A,B) => C):  Future[C] = {

    UnitFuture(f(af.get, bf.get))    // Stub implemetation.
  }
}
