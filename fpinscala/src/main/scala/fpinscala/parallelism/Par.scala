package fpinscala.parallelism

import java.util.concurrent.Future
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.CancellationException
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeoutException

/** Par object.
 *
 *  For now Par is just a type alias.
 *
 *  The members of the Par companion object allow
 *  parallel calculations to be defined in a pure way.
 *  Par.run will produce a function that when given a
 *  java.util.concurrent.ExecutorService will produce
 *  a java.util.concurrent.Future.
 *
 *  Usage in code:
 *    import fpinscala.parallelism.Par._
 *
 */
final object Par {

  /** Par type alias. */
  type Par[A] = ExecutorService => Future[A]

  /** Par.fork marks a calculation, in the resulting Future, to be
   *  done in a parallel thread.
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
   *  implementation of fork would still work in Java 8.
   *
   */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(() => a(es).get)

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
    */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  /** Wrap a constant value in a Par. 
   *
   *  Does not actually use the underlying java or OS
   *  multi-threading mechanisms.  The Par it returns
   *  is a constant function not depending on the
   *  (es: ExecutorService) passed to it.  
   *
   */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /** Lazy version of unit
   *
   *  Evaluates its argument in a separate thread,
   *  only if required.
   *
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Evaluate a function asynchronously */
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  /** Par.map2 combines two parallel computations with a function.
   *
   *  Function not evaluated in a separate thread.  To
   *  do that, use `fork(map2(a,b)(f))'
   *
   *  Looking at the book's answer key solution, the 
   *  trick is to wrap the futures I am passed into
   *  another future object, Map2Future.
   *
   */
  def map2[A,B,C]( a: Par[A]
                 , b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  /** Map a function into a parallel calculation.  */
  def map[A,B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a,_) => f(a))

  /** Combine three parallel computations with a function. */
  def map3[A,B,C,D]( a: Par[A]
                   , b: Par[B]
                   , c: Par[C])(f: (A,B,C) => D): Par[D] = 
    map2(map2(a, b)((_, _)), c)((p, c) => f(p._1, p._2, c))

  /** Combine four parallel computations with a function. */
  def map4[A,B,C,D,E]( a: Par[A]
                     , b: Par[B]
                     , c: Par[C]
                     , d: Par[D])(f: (A,B,C,D) => E): Par[E] = 
    map2(map2(a, b)((_, _)), map2(c, d)((_, _))) {
      (p1, p2) => f(p1._1, p1._2, p2._1, p2._2 )
    }

  /** Combine five parallel computations with a function. */
  def map5[A,B,C,D,E,F]( a: Par[A]
                       , b: Par[B]
                       , c: Par[C]
                       , d: Par[D]
                       , e: Par[E])(f: (A,B,C,D,E) => F): Par[F] = 
    map2(map3(a, b, c)((_, _, _)), map2(d, e)((_, _))) {
      (t, p) => f(t._1, t._2, t._3, p._1, p._2)
    }

  /** Change an IndexedSeq of pars into a par of a IndexedSeq. */
  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    fork {
      if (ps.isEmpty)
        unit(Vector())
      else if (ps.length == 1)
        map(ps.head)(a => Vector(a))
      else {
        val (lps,rps) = ps.splitAt(ps.length/2)
        map2(sequenceBalanced(lps), sequenceBalanced(rps))(_ ++ _)
      }
    }

  /** Change a List of pars into a par of a List. */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  /** Create a calcultion to map over a list in parallel
   * 
   *  Book sugggestion regarding the fork.  The parMap function
   *  will return immediately even for very long lists.
   *
   */
  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = 
    fork(sequence(as.map(asyncF(f))))

  /** Filter elements of a list in parallel. */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    def ff(a: A): Option[A] =
      if (f(a)) Some(a)
      else None

    map(parMap(as)(ff))(_.foldRight(Nil: List[A])((aa, aas) =>
      aa match {
        case None => aas
        case Some(a) => a :: aas
      }))

  }

  //
  // Par private Future helper classes:
  //

  /** UnitFuture helper class for Par.unit method.
   *
   *  Does not use the underlying es (ExecutorService),
   *  basically, born done.
   *
   */
  private final
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
  private final
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
}
