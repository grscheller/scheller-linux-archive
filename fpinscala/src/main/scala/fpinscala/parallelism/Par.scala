package fpinscala.parallelism

import java.util.concurrent.{Future,ExecutorService}
import java.util.concurrent.{Callable,TimeUnit,CancellationException}

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

    def isDone: Boolean = true

    def isCancelled: Boolean = false

    def cancel(evenIfRunning: Boolean): Boolean = false

  }

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
   *  for adjusting timeouts to the parallel calculation.
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

  /** Map2Future helper class for Par.map2 method.
   *
   *  Wrap the two futures into another future so
   *  that f can be evaluated after get is called.
   *
   *  Design Choices:
   *  1. Final value cached, implementation prevents repeated
   *     re-evaluations.
   *  2. Use case f pure. If f has side effects, they will only
   *     happen the first time a get method on the future is called. 
   *  3. Likewise, any side effects of futures af and bf are 
   *     similarly surpressed.
   *  4. If f is a long running function, the time out contract
   *     can still be violated.
   *  5. Ability to "uncancel" itself in the event an actual value
   *     is eventually computed.  This could conceivably happen if
   *     f is long running and/or lazy in one of its arguments.
   *  6. @throws annontations for Java capatibility.
   *  7. Trying to make this future usable to Java clients, or
   *     more iperitive Scala code, makes its implementation more
   *     problematic.
   *     
   */
  private final
  case class Map2Future[A,B,C](af: Future[A],
                               bf: Future[B],
                                f: (A,B) => C
                              ) extends Future[C] {

    // Internal state -
    private var done: Boolean = false
    private var cancelled: Boolean = false
    private var value: Option[C] = None

    @throws(classOf[CancellationException])
    def get(): C =
      if (cancelled)
        throw new CancellationException()
      else
        value getOrElse {
          value = Some(f(af.get, bf.get))
          done = true
          value.get
        }

    @throws(classOf[CancellationException])
    def get(timeout: Long, units: TimeUnit): C =
      if (cancelled)
        throw new CancellationException()
      else
        value getOrElse {
          val timeoutNS = TimeUnit.NANOSECONDS.convert(timeout, units)
          val t0 = System.nanoTime
          val av = af.get(timeoutNS, TimeUnit.NANOSECONDS)
          val t1 = System.nanoTime
          val newTimeOut1 = timeoutNS - (t1 - t0)
          val bv = bf.get(timeoutNS - (t1 - t0), TimeUnit.NANOSECONDS)
          value = Some(f(av, bv))
          done = true
          value.get
        }

    def isDone = done

    def isCancelled = cancelled

    def cancel(evenIfRunning: Boolean): Boolean = {
      if (done) {
        cancelled = false  // Fix the unlikely race condition
        cancelled          // if we actually got a result?
      } else if (cancelled) {
        cancelled
      } else {
        // Not sure about cancelling these underlying futures.
        // In functional code that gets handed a passed down
        // es, yes, for efficiency reasons.
        // In imperitive code, other enities, unrelated to
        // this particular future, could still use them.
        val af_cancelled = af.cancel(evenIfRunning)
        val bf_cancelled = bf.cancel(evenIfRunning)
        cancelled = af_cancelled || bf_cancelled
        cancelled
      }
    }

  }
}
