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
      val finalValue = f(av, bf.get(timeoutNS - (t1 - t0), TimeUnit.NANOSECONDS))
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

    /** Returns true if completed or has been cancelled.
     *
     *    To facilitate the use case of testing the future's 
     *    isDone method in an event loop, kickoff a calculation
     *    if the isDone method called before one of the get
     *    methods are called.
     *
     *    Calculation is done in another thread so isDone 
     *    does not block.
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

    /** Cancel the future.
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
      } else {
          val af_cancelled = af.cancel(evenIfRunning)
          val bf_cancelled = bf.cancel(evenIfRunning)
          cancelled = af_cancelled || bf_cancelled
          if (cancelled) {
            done = true
            true
          } else false
      }
  }
}
