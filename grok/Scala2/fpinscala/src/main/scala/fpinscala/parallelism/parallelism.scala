/** Parallel calculations package.
  *
  *    The Par trait is used to define a parallel calculation
  *    and once defined, perform it.
  *
  *    The Par.run method blocks and returns a value.
  */
package fpinscala.parallelism

import java.util.concurrent.{CountDownLatch, Future}
import java.util.concurrent.{ExecutorService => ES}
import java.util.concurrent.atomic.AtomicReference

/** Register a "Callback" for a parallel calculation
  *
  *    Made private due to unit, fork, map2, flatMap
  *    needing to know how to create ParFutures.
  *
  *    Made private[parallelism] to potentially allow other
  *    code in this package to provide an API for clients
  *    to register multiple callbacks.
  */
private[parallelism] sealed trait ParFuture[+A] {
  def apply(cb: A => Unit, onError: Throwable => Unit): Unit
}

/** Par Trait. */
sealed trait Par[+A] { self =>

  import Par._

  def apply(es: ES): ParFuture[A]

  /** Perform the parallel calculation described by the Par.
    *
    *    Returns the final value.
    *    Blocks until value is available.
    *
    *    Throws exception, which client is responsible to catch,
    *    when an error condition occurs.
    */
  def run(es: ES): A = {

    var exceptionThrown: Option[Throwable] = None
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    val onError: Throwable => Unit =
      (ex: Throwable) => {
        exceptionThrown = Some(ex)
        latch.countDown
      }

    self(es)(
      { a =>
        ref.set(a)
        latch.countDown
      },
      onError
    )

    latch.await
    exceptionThrown map (throw _) getOrElse ref.get
  }

  /** flatMap - implemented as a new primitive
    *
    *  The only way to get an `A' to apply f is to
    *  call a Par's run method.  Abstracts the idea
    *  of producing a new Par from the result of
    *  another Par.
    */
  def flatMap[B]: (A => Par[B]) => Par[B] = flatMapPrimitive

  // flatMap implementations:

  /** flatMap - implemented as a new primitive. */
  def flatMapPrimitive[B](f: A => Par[B]): Par[B] =
    new Par[B] {
      def apply(es: ES) =
        new ParFuture[B] {
          def apply(cb: B => Unit, onError: Throwable => Unit): Unit =
            try {
              cb(f(self.run(es)).run(es))
            } catch {
              case ex: Throwable => onError(ex)
            }
        }
    }

  /** flatMap - implemented join. */
  def flatMapViaJoin[B](f: A => Par[B]): Par[B] = join(self.map(f))

  /** Combine two parallel computations with a function.
    *
    *  Function not evaluated in a separate thread.  To
    *  do that, use `fork(map2(a,b)(f))'
    */
  def map2[B, C]: Par[B] => ((A, B) => C) => Par[C] = map2ViaActor

  // map2 implementations:

  /** Map2 implementation via flatMap.
    *
    *  Beautifully simple, but poor parallel performance.
    *  The problem is that we have a blocking ParFuture
    *  wrapped in another blocking ParFuture.
    *  1. Serializes any calculation that uses it.
    *  2. Deadlocks on fixed size threadpools with
    *     less than 4 threads.
    */
  def map2ViaFlatMap[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    self.flatMap { (a: A) => pb.flatMap { (b: B) => unit(f(a, b)) } }

  /** Map2 implementation via actors.
    *
    *  Non-blocking implementation of map2 where the arguments
    *  to f are evaluated in parallel.  Threadsafe.
    */
  def map2ViaActor[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    new Par[C] {
      def apply(es: ES) =
        new ParFuture[C] {
          def apply(cb: C => Unit, onError: Throwable => Unit): Unit = {

            var al: Option[A] = None
            var br: Option[B] = None

            val combiner = Actor[Either[A, B]](es)(
              {
                case Left(a) =>
                  br match {
                    case None    => al = Some(a)
                    case Some(b) => eval(es)(cb(f(a, b)), onError)
                  }
                case Right(b) =>
                  al match {
                    case None    => br = Some(b)
                    case Some(a) => eval(es)(cb(f(a, b)), onError)
                  }
              },
              onError
            )

            self(es)({ a => combiner ! Left(a) }, onError)
            pb(es)({ b => combiner ! Right(b) }, onError)

          }
        }
    }

  // map2 related methods:

  /** Map a function into a parallel calculation. */
  def map[B](f: A => B): Par[B] =
    self.map2(unit(())) { (a, _) => f(a) }

  /** Map a function into a parallel calculation. */
  def mapViaFlatMap[B](f: A => B): Par[B] =
    self.map2ViaFlatMap(unit(())) { (a, _) => f(a) }

  /** Combine three parallel computations with a function. */
  def map3[B, C, D](pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    self.map2(pb)((_, _)).map2(pc) { (t, c) => f(t._1, t._2, c) }

  /** Combine four parallel computations with a function. */
  def map4[B, C, D, E](pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] =
    self.map3(pb, pc)((_, _, _)).map2(pd) { (t, d) => f(t._1, t._2, t._3, d) }

  /** Combine five parallel computations with a function. */
  def map5[B, C, D, E, F](pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    self.map4(pb, pc, pd)((_, _, _, _)).map2(pe) { (t, e) => f(t._1, t._2, t._3, t._4, e) }
}

/** Par companion object */
object Par {

  /** fork and map2 helper function,
    *
    *  Returns value of type java.util.concurrent.Future[Unit],
    *  which we totally ignore.  The asychronous value is returned
    *  via a side effect in the Par.run method.
    */
  private def eval[A](
      es: ES
  )(r: => Unit, onError: Throwable => Unit): Future[Unit] =
    es.submit[Unit] { () =>
      try {
        r
      } catch {
        case ex: Throwable => onError(ex)
      }
    }

  /** Par.fork marks a calculation to be done in a parallel thread. */
  def fork[A](pa: => Par[A]): Par[A] =
    new Par[A] {
      def apply(es: ES) =
        new ParFuture[A] {
          def apply(cb: A => Unit, onError: Throwable => Unit) =
            eval(es)(pa(es)(cb, onError), onError)
        }
    }

  /** Wrap a constant value in a Par.
    *
    *  Does not actually use the underlying java or OS
    *  multi-threading mechanisms.  The Par it returns
    *  does not depending on the (es: ES) passed to the
    *  run method.
    */
  def unit[A](a: A): Par[A] =
    new Par[A] {
      def apply(es: ES) =
        new ParFuture[A] {
          def apply(cb: A => Unit, onError: Throwable => Unit) =
            try {
              cb(a)
            } catch {
              case ex: Throwable => onError(ex)
            }
        }
    }

  /** join (flatten)
    *
    *    join as in threads, flatten as in monads
    */
  def join[A]: Par[Par[A]] => Par[A] = joinViaFlatMap

  /** join via run
    *
    *  Block on outside Par
    *
    *  run(es) to get a Par[A] back,
    *  then apply es again so that the
    *  new Par's apply method gives
    *  back a ParFuture[A].
    */
  def joinBlockingOutside[A](ppa: Par[Par[A]]): Par[A] =
    new Par[A] {
      def apply(es: ES) = (ppa.run(es))(es)
    }

  /** join via map
    *
    *  Block on inside Par
    *
    *  run(es) on the inside Par[A]
    *  to return an A making the
    *  outside Par a Par[A]
    */
  def joinBlockingInside[A](ppa: Par[Par[A]]): Par[A] =
    new Par[A] {
      def apply(es: ES) = (ppa map (_.run(es)))(es)
    }

  /** join via flatMap
    *
    *  Really that simple???
    */
  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    ppa flatMap { (pa: Par[A]) => pa }

  /** Lazy version of unit
    *
    *  Evaluates its argument in a separate thread,
    *  only if required.
    */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /** Delay instantiation of part of a computation
    *  until run time, and then only if needed.
    */
  def delay[A](pa: => Par[A]): Par[A] =
    new Par[A] {
      def apply(es: ES) = pa(es)
    }

  /** Evaluate a function asynchronously. */
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    p1.map2(p2) { _ == _ }

  /** Apply a binary operator across an indexable collection in parallel.
    *
    *    Note: For efficiency, collection assumed nonempty.
    */
  def balancedBinComp[A](ps: IndexedSeq[Par[A]])(binOp: (A, A) => A): Par[A] = {

    def balanced(pars: IndexedSeq[Par[A]]): Par[A] = fork {
      if (pars.size == 1)
        pars(0)
      else {
        val (lpars, rpars) = pars.splitAt(pars.size / 2)
        balanced(lpars).map2(balanced(rpars))(binOp)
      }
    }

    balanced(ps)
  }

  /** Change an IndexedSeq of Pars into a Par of an IndexedSeq. */
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
    */
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(as map (asyncF(f))))

  /** Filter elements of a list in parallel. */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    def ff(a: A): Option[A] =
      if (f(a)) Some(a)
      else None

    parMap(as)(ff) map {
      _.foldRight(Nil: List[A]) {
        (aa, aas) => aa match {
            case None    => aas
            case Some(a) => a :: aas
        }
      }
    }
  }

  /** Boolean choice function via flatMap */
  def choice[A](pred: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    pred.flatMap {
      case true  => t
      case false => f
    }

  /** Boolean choice function via flatMapViaJoin */
  def choiceViaJoin[A](pred: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    pred.flatMapViaJoin {
      case true  => t
      case false => f
    }
}
