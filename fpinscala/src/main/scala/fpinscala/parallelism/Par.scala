package fpinscala.parallelism

import java.util.concurrent.{Future,ExecutorService}
import java.util.concurrent.{Callable,TimeUnit}

/** Par object.
 *
 */
object Par {

  type Par[A] = ExecutorService => Future[A]

  /** Wrap a constant value in a Par. 
   *
   *  Does not actually use the underlying java or OS
   *  multi-threading mechanisms.  The Par it returns
   *  is a constant function of whatever
   *  (es: ExecutorService) passed to it.  
   *
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /** A Future for wrapping a constant value.
   *
   *  This version does not respect the Future's contract
   *  to throw an exception after a specified time
   *
   */
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = true
  }

  /** lazy version of unit */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Return a Future for a parallel calculation.
    *
    * Book page 105 and book answers both have the return
    * type being Future[A].  So, run is not returning
    * the final value of type A, but an imperitive little
    * java beasty called a Future  You will need to use
    * its get method to actually get the value of type A.
    *
    * So, functional code which returns a Future[A].  This
    * could be useful if you had to pass it to existing
    * java code.
    *
    * In scala code, I would guess best practices would be
    * to push these Futures to the "outside edge" of the
    * program.
    *
    */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /** Combined two parallel computations with a function.
   *
   *  Function not evaluated in a separate thread.  To
   *  do that, use
   *
   *     fork((map2(a,b), f))
   *
   */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  /** Mark a calculation to be done in a parallel thread
   *  when the resulting Future is eventially evaluated.
   */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] { def call = a(es).get })

}
