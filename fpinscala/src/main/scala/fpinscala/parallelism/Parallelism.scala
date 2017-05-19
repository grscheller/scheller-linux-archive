/** Parallel calculations package.
 *
 *    The Par trait is used to define a parallel calculation
 *    and once defined, perform it.
 *
 *    The Par.run method blocks and returns a value.
 *
 *    Current version has deadlocking issues with threadpools
 *    with a fixed number of threads.  (Bit of a thread hog.)
 *
 */
package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit}
import java.util.concurrent.{CancellationException, ExecutionException}
import java.util.concurrent.TimeoutException

/** Par Trait. */
sealed trait Par[A] { self =>

  import Par._

  /** Internally, Par behaves like a function. */
  private[parallelism] def apply(es: ExecutorService): Future[A]

  /** Perform the parallel calculation described by the Par.
   *  
   *    Returns the final value.
   *    Blocks until value is available.
   *
   */
  def run(es: ExecutorService): A = this(es).get

  /** Combine two parallel computations with a function.
   *
   *  Function not evaluated in a separate thread.  To
   *  do that, use `fork(map2(a,b)(f))'
   *
   */
  def map2[B,C](pb: Par[B])(f: (A,B) => C): Par[C] =
    new Par[C] {
      def apply(es: ExecutorService) = {
        val af = self(es)
        val bf = pb(es)
        Map2Future(af, bf, f)
      }
    }

  // map2 related methods:

  /** Map a function into a parallel calculation.  */
  def map[B](f: A => B): Par[B] =
    this.map2(unit(())) {
      (a, _) => f(a)
    }

  /** Combine three parallel computations with a function. */
  def map3[B,C,D]( pb: Par[B]
                 , pc: Par[C])(f: (A,B,C) => D): Par[D] = 
    this.map2(pb)((_,_)).map2(pc) {
      (t, c) => f(t._1, t._2, c)
    }

  /** Combine four parallel computations with a function. */
  def map4[B,C,D,E]( pb: Par[B]
                   , pc: Par[C]
                   , pd: Par[D])(f: (A,B,C,D) => E): Par[E] = 
    this.map3(pb, pc)((_,_,_)).map2(pd) {
      (t, d) => f(t._1, t._2, t._3, d)
    }

  /** Combine five parallel computations with a function. */
  def map5[B,C,D,E,F]( pb: Par[B]
                     , pc: Par[C]
                     , pd: Par[D]
                     , pe: Par[E])(f: (A,B,C,D,E) => F): Par[F] = 
    this.map4(pb, pc, pd)((_,_,_,_)).map2(pe) {
      (t, e) => f(t._1, t._2, t._3, t._4, e)
    }

}

/** Par companion object */
object Par {

  /** Par.fork marks a calculation to be done in a parallel thread.  */
  def fork[A](pa: => Par[A]): Par[A] =
    new Par[A] {
      def apply(es: ExecutorService) = es.submit(() => pa(es).get)
    }
 
  /** Wrap a constant value in a Par. 
   *
   *  Does not actually use the underlying java or OS
   *  multi-threading mechanisms.  The Par it returns
   *  does not depending on the (es: ExecutorService)
   *  passed to the run method.  
   *
   */
  def unit[A](a: A): Par[A] =
    new Par[A] {
      def apply(es: ExecutorService) = UnitFuture(a)
    }

  /** Lazy version of unit
   *
   *  Evaluates its argument in a separate thread,
   *  only if required.
   *
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Evaluate a function asynchronously. */
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  /** Delay instantiation of a computation,
   *  at run time, only if needed.
   */
  def delay[A](pa: => Par[A]): Par[A] =
    new Par[A] {
      def apply(es: ExecutorService) = pa(es)
    }

  /** Apply a binary operator across an indexable collection in parallel.
   *
   *    Note: For efficiency, collection assumed nonempty.
   */
  def balancedBinComp[A](ps: IndexedSeq[Par[A]])(binOp: (A,A) => A): Par[A] = {

    def balanced(pars: IndexedSeq[Par[A]]): Par[A] =
      if (pars.size == 1)
        fork(pars(0))
      else {
        val (lpars, rpars) = pars.splitAt(pars.size/2)
        balanced(lpars).map2(balanced(rpars))(binOp)
      }

    fork(balanced(ps))
  }

  /** Change an IndexedSeq of Pars into a Par of an IndexedSeq.  */
  def sequenceIndexedSeq[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if (ps.isEmpty)
      unit(IndexedSeq())
    else {
      val pv = ps.map(_.map(a => IndexedSeq(a)))
      balancedBinComp(pv)(_ ++ _)
    }

  /** Change a List of pars into a par of a List. */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    sequenceIndexedSeq(ps.toVector) map (_.toList)

  /** Create a calcultion to map over a list in parallel
   * 
   *  Book sugggestion regarding the fork.  The parMap function
   *  will return immediately even for very long lists.
   *
   */
  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = 
    fork(sequence(as map (asyncF(f))))

  /** Filter elements of a list in parallel. */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    def ff(a: A): Option[A] =
      if (f(a)) Some(a)
      else None

    parMap(as)(ff) map {
      _.foldRight(Nil: List[A]) {
        (aa, aas) =>
          aa match {
            case None => aas
            case Some(a) => a :: aas
          }
      }
    }
  }

}

// Will morph this into to something that does not extend Future
private[parallelism] sealed
trait ParFuture[A] extends Future[A] {
  private[parallelism]
  def apply(k: A => Unit): Unit
  def isCancelled: Boolean = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}

/** UnitFuture helper class for Par.unit method.
  *
  *  Does not use the underlying es (ExecutorService),
  *  basically, born done.
  *
  */
  private[parallelism] 
  case class UnitFuture[A](get: A) extends ParFuture[A] {
    def apply(k: A => Unit): Unit = (k: A) => println("<<<UnitFuture>>>")
    def get(timeout: Long, units: TimeUnit): A = get
    def isDone: Boolean = true
  }

/** Map2Future helper class for Par.map2 method.
 *
 *  Wrap the two futures into another future so
 *  that f can be evaluated after get is called.
 *
 */
private[parallelism] 
case class Map2Future[A,B,C]( af: Future[A]
                            , bf: Future[B]
                            ,  f: (A,B) => C
                            ) extends ParFuture[C] {

  var done: Boolean = false
  @volatile private var value: Option[C] = None

  def apply(k: C => Unit): Unit = (k: C) => println("<<<Map2Future>>>")

  def get(): C =
    value getOrElse {
      value = Some(f(af.get, bf.get))
      done = true
      value.get
    }
  def get(timeout: Long, units: TimeUnit): C = get
  def isDone: Boolean = done

}
