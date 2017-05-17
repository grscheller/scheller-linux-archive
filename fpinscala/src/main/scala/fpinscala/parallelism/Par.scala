package fpinscala.parallelism

import java.util.concurrent.Future
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.CancellationException
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeoutException

/** Trait used to define calculations with parallelism. */
sealed trait Par[A] { self =>

  import Par._

  /** Internally, Par behaves like a function. */
  private[parallelism] def apply(es: ExecutorService): Future[A]

  /** Perform the parallel calculation described by the Par.
   *
   *  The run method does not return the final value of
   *  type A, but an imperitive java wee beasty of type
   *  Future[A].  You will need to use its get method
   *  to actually get the value of type A.
   *
   */
  def run(es: ExecutorService): Future[A] = this(es)

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

  /** Par.fork marks a calculation to be done in a parallel thread.
   *
   *  In Java 8+, Future now has a functional interface. 
   *
   *  Before java 8, I would have had to pass a
   *    new Callable[A] { def call = a(es).get }
   *  anonymous inner class to the es submit method.
   *
   */
  def fork[A](pa: => Par[A]): Par[A] =
    new Par[A] {
      def apply(es: ExecutorService) =
        es.submit(() => pa(es).get)
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
  def balancedBinComp[A](ps: IndexedSeq[Par[A]])(binOp: (A,A) => A): Par[A] =
    fork {
      if (ps.size == 1)
        ps(0)
      else {
        val (lps,rps) = ps.splitAt(ps.size/2)
        balancedBinComp(lps)(binOp).map2(balancedBinComp(rps)(binOp))(binOp)
      }
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
  def parFilter1[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

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

  /** Filter elements of a list in parallel - Books version. */
  def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as map {
      asyncF(a => if (f(a)) List(a) else List())
    }
    sequence(pars) map (_.flatten)
  }

  /** Filter elements of a list in parallel. */
  def parFilter[A] = parFilter1[A] _

}

/** UnitFuture helper class for Par.unit method.
  *
  *  Does not use the underlying es (ExecutorService),
  *  basically, born done.
  *
  */
 private[parallelism] 
 case class UnitFuture[A](get: A) extends Future[A] {
   def get(timeout: Long, units: TimeUnit): A = get
   def isDone: Boolean = true
   def isCancelled: Boolean = false
   def cancel(evenIfRunning: Boolean): Boolean = false
  }

/** Map2Future helper class for Par.map2 method.
 *
 *  Wrap the two futures into another future so
 *  that f can be evaluated after get is called.
 *
 *  Design Choices:
 *  1. Final value cached, implementation prevents repeated
 *     re-evaluations.
 *  2. If f is a long running function, the time out contract
 *     can still be violated.
 *  3. The future doesn't start computing its value until either:
 *     - A blocking get method is called.
 *     - The isDone method is involked.
 *     The later case is to enable the common Java use case
 *     of using an event loop to periodically check the
 *     future's isDone method.
 *  4. Follows Java Future API more closely than book answerkey version.
 *     Book's version violates both isDone and cancel contracts.
 *  5. @throws annontations are for Java capatibility.
 *     
 */
private[parallelism] 
case class Map2Future[A,B,C]( af: Future[A]
                            , bf: Future[B]
                            ,  f: (A,B) => C
                            ) extends Future[C] {

  // Internal state:
  private var hasStarted: Boolean = false
  private var cancelled: Boolean = false
  private var done: Boolean = false
  @volatile private var value: Option[C] = None

  // Perform the calculation
  private 
  def calculate(): Unit = {
    hasStarted = true
    val finalValue = f(af.get, bf.get)
    value = Some(finalValue)
    done = true
  }

  // Perform the calculation with timeouts
  private 
  def calculate(timeout: Long, units: TimeUnit): Unit = {
    hasStarted = true
    val timeoutNS = TimeUnit.NANOSECONDS.convert(timeout, units)
    val t0 = System.nanoTime
    val av = af.get(timeoutNS, TimeUnit.NANOSECONDS)
    val t1 = System.nanoTime
    val finalValue =
      f(av, bf.get(timeoutNS - (t1 - t0), TimeUnit.NANOSECONDS))
    value = Some(finalValue)
    done = true
  }

  @throws(classOf[CancellationException])
  @throws(classOf[ExecutionException])
  @throws(classOf[InterruptedException])
  def get(): C =
    if (cancelled)
      throw new CancellationException()
    else
      value getOrElse {
        calculate()
        if (cancelled)
          throw new CancellationException()
        else
          value.get
      }

  @throws(classOf[CancellationException])
  @throws(classOf[ExecutionException])
  @throws(classOf[InterruptedException])
  @throws(classOf[TimeoutException])
  def get(timeout: Long, units: TimeUnit): C =
    if (cancelled)
      throw new CancellationException()
    else
      value getOrElse {
        calculate(timeout, units)
        if (cancelled)
          throw new CancellationException()
        else
          value.get
      }

  /** Map2Future.isDone returns true if completed or has been cancelled.
   *
   *    To facilitate the use case of testing the future's 
   *    isDone method in an event loop, kickoff a calculation
   *    if the isDone method called before one of the get
   *    methods are called.
   *
   *    Calculation is done in another thread so isDone 
   *    does not block.  Can't use Par.fork directly here
   *    unless I explicitly pass the es to the Map2Future
   *    case class.
   *
   *    Swallows all exceptions for compatibility with the
   *    Java Futures trait.
   *
   */
  def isDone: Boolean = {
    if ( ! hasStarted && ! isCancelled )
      new Thread( () =>
        try {
          calculate()
        } catch {
          case ex: TimeoutException      => 
          case ex: CancellationException => 
          case ex: InterruptedException  => 
          case ex: ExecutionException    => 
        } ).start()
    done
  }

  /** Test if future cancelled.
   *
   *  Returns true if future successfully
   *  cancelled before completion.
   *
   */
  def isCancelled: Boolean = cancelled

  /** Map2Future.cancel cancels the future.
   *
   *  The return value means the command was
   *  successful, not the isCancelled state.
   *
   *  Note: Futures af and bf are not visible to code outside
   *        the context of the Par in which they are defined.
   *
   *  Note: From the Java API
   *     1. Can't cancel if done.
   *     2. Return false if already cancelled.
   *     3. If future is cancelled, its isDone
   *        method returns true.
   */
  def cancel(evenIfRunning: Boolean): Boolean =
    if (done) {
        false
    } else if (cancelled) {
        false
    } else if (! hasStarted) {
        cancelled = true
        done = true
        af.cancel(evenIfRunning)
        bf.cancel(evenIfRunning)
        true
    } else if (evenIfRunning) {
        val af_cancelled = af.cancel(true)
        val bf_cancelled = bf.cancel(true)
        cancelled = af_cancelled || bf_cancelled
        if (cancelled) {
          done = true
          true
        } else false
    } else false

}
