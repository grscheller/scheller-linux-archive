package grockScala.laziness

/** Implement a lazy list
 *
 * @author    "Geoffrey Scheller <geoffrey@scheller.com>"
 * @version   1.0
 * @since     1.0
 */
sealed trait Stream[+A] {

  import Stream._

  /*  For for expressions
   *
   *  This imperitive hook maybe out of place 
   *  in this bit of functional heaven the book is
   *  trying to create.
   *
   *  To fully implement for comprehensions and expressions,
   *  you need to implement all the members in the trait
   *  scala.collection.generic.FilterMonadic.
   *
   *  These are: flatMap and map (for comprehensions), 
   *             foreach (for for expressions), and
   *             withFilter (to enable guards).
   *
   *  If withFilter is not defined, the compiler will use the
   *  filter method for guards in its place but will complain
   *  about it. Filter creates an intermediate data structure.  
   *  An actual withFilter method wraps the monad in a withFilter
   *  object which lazily filters out the undesired elements from
   *  the original flatMap, map, and foreach method calls.
   *
   */
  def foreach[U](f: A => U): Unit = this match {
    case Cons(h, t) => {f(h()); t().foreach(f)}
    case Empty => ()
  }

  def headOption1: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())   // Explicitly evaluate the thunk
  }

  def tailOption: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(h, t) => Some(t())
  } 

  def tailSafe: Stream[A] =
    tailOption getOrElse (empty[A])

  def toList1: List[A] = this match {
    case Cons(h, t) => h() :: t().toList1
    case Empty => Nil
  }

  def drop(n: Int): Stream[A] = 
    if (n < 1) this
    else this match {
      case Cons(_, t) => t().drop(n-1)
      case _ => Empty
    }

  def dropWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => t().dropWhile(p)
      case _ => this
    }

  def take(n: Int): Stream[A] = 
    if (n < 1) Empty
    else this match {
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => Empty
    }

  def takeWhile1(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile1(p))
      case _ => Empty
    }

  def exists1(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists1(p)
      case _ => false
    }

  // 
  /** Reduce a stream right to left with a function.
   *
   *  @param z initial value of the accumulator (value
   *           for empty stream)
   *  @param f folding function applied to stream
   *  @return A Stream of values
   *  @note Return value generated from stream elements
   *        right to left.
   *  @note Second parameter to f not strict, allows for
   *        early termination from the recursion.
   *  
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => 
      if (p(a)) cons(a, b)
      else Empty)

  def headOption: Option[A] = 
    foldRight(None: Option[A])((a, _) => Some(a))

  def toList: List[A] =
    foldRight(Nil: List[A])((a, as) => a :: as.toList)

  def mapFR[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, bs) => cons(f(a), bs))

  def map[B](f: A => B): Stream[B] = {
    def uf(s: Stream[A]): Option[(B, Stream[A])] = s match {
      case Cons(a, as) => Some( (f(a()), as()) )
      case Empty => None
    }
    unfold(this)(uf)
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, as) =>
      if (p(a)) cons(a, as)
      else as )

  /** Strict operator wrapper for cons */
  def #::[B>:A](b: B): Stream[B] =
    cons(b, this)

  /** Prepend supertype stream */
  def #:::[B>:A](bs: Stream[B]): Stream[B] =
    bs.foldRight(this: Stream[B])((b,bs) => cons(b, bs))

  /** Append supertype stream */
  def :::#[B>:A](bs: Stream[B]): Stream[B] =
    foldRight(bs)((a,as) => cons(a, as))          // Book ans way

  /** Flatmap (Bind) for Streams */
  def flatMap1[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, bs) => f(a) #::: bs)  // Scala library way

  /** Flatmap (Bind) for Streams */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, bs) => f(a) :::# bs)  // Book answer way

  /** Return the first element which matches a predicate */
  def find(p: A => Boolean) =
    filter(p).headOption

  def zipWith[B,C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Cons(a, as), Cons(b, bs)) =>
        Some((f(a(), b()), (as(), bs())))
      case _ =>
        None
    }

  def zip[B](that: Stream[B]): Stream[(A,B)] =
    zipWith(that)((_,_))

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(a, as), Cons(b, bs)) =>
        Some((Some(a()), Some(b())), (as(), bs()))
      case (Empty, Cons(b, bs)) =>
        Some((None, Some(b())), (empty, bs()))
      case (Cons(a, as), Empty) =>
        Some((Some(a()), None), (as(), empty))
      case _ =>
        None
    }

  // Had to add "final" otherwise compiler would not allow tail
  // recursion optimization due to the possibility of overiding
  // in a derived class. (Once done, annotation not needed - but
  // I needed it to diagnose the problem.)
  @annotation.tailrec
  final def startsWith[B>:A](prefix: Stream[B]): Boolean =
    (this, prefix) match {
      case (Cons(a, as), Cons(b, bs)) =>
        if (a() == b()) as().startsWith(bs())
        else false
      case (_, Empty) =>
        true
      case (Empty, _) =>
        false
    }

  // Not stacksafe (due to foldRight), but pretty.
  // FAILS: When prefix stream is longer than stream,
  //        really need zipAll, book answers also implements
  //        a zipAllWith.
  def startsWith2[B>:A](prefix: Stream[B]): Boolean =
    zipWith(prefix)(_ == _).foldRight(true)(_ && _)

  // Book answer version - also not stacksafe
  //   Sugared match statement below compares Options.
  def startsWith3[B>:A](prefix: Stream[B]): Boolean =
    zipAll(prefix).takeWhile(_._2.nonEmpty) forAll {
      case (hO1, hO2) => hO1 == hO2
    }

  // Simple, but final empty list handled
  // like an edge case in imperative code.
  def tails1(): Stream[Stream[A]] =
    unfold(this)((s: Stream[A]) => s match {
      case Cons(_, rest) => Some((s, rest()))
      case _ => None
    }) #::: Stream(Empty)

  // State now handled as an Option[Stream[A]] so
  // that I have a state to represent no tail.
  def tails(): Stream[Stream[A]] =
    unfold(Some(this): Option[Stream[A]])(
      (os: Option[Stream[A]]) => os flatMap {
        case Cons(_, rest) => Some((os.get, Some(rest())))
        case Empty => Some((os.get, None))
      }
    )

  def hasSubsequence[B](sub: Stream[B]): Boolean =
    tails exists (_ startsWith sub)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  /** Stream smart constructor */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /** Empty Stream smart constructor */
  def empty[A]: Stream[A] = Empty

  /** Variadic strict stream constuctor */
  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))

  /** Convert List to Stream */
  def listToStream[A](l: List[A]): Stream[A] =
    l.foldRight(empty[A])((a, s) => cons(a, s))

  /** Take an initial state and generate a stream
   *  by repeatedly applying a function, which can
   *  fail, to the current state.
   *
   *  @param s the initial state
   *  @param f takes a state and returns an Option to
   *         a value and state Pair
   *  @return A Stream of values
   *  @note The Option is used to determine when to
   *        terminate the stream, if ever.
   *  @note Elements in stream are generated left to right.
   *  
   */
  def unfold[A,S](s: S)(f: S => Option[(A, S)]): Stream[A] =
    f(s) map (p => cons(p._1, unfold(p._2)(f))) getOrElse empty

  /** Books version of my unfold function */
  def unfold1[A,S](s: S)(f: S => Option[(A, S)]): Stream[A] =
    f(s) match {
      case Some((a, s)) => cons(a, unfold1(s)(f))
      case None => empty
    }

  /* Range related operators - just implement for Int
     for now. */

  /** Count up from start by inc */
  def from(start: Int, inc: Int): Stream[Int] =
    cons(start, from(start + inc, inc))

  /** Count up from start */
  def from(start: Int): Stream[Int] =
    from(start, 1)

  //** Exclusive range with increment */
  def range(start: Int, stop: Int, inc: Int): Stream[Int] =
    if (stop >= start)
      from(start, inc).takeWhile(_ < stop)
    else
      from(start, inc).takeWhile(_ > stop)

  //** Exclusive range */
  def range(start: Int, stop: Int): Stream[Int] =
    if (stop >= start)
      range(start, stop, 1)
    else
      range(start, stop, -1)

  /** Count up from start using unfold */
  def fromu(start: Int): Stream[Int] =
    unfold(start)(s => Some((s, s + 1)))

  /** Count up from start using unfold1 */
  def fromu1(start: Int): Stream[Int] =
    unfold1(start)(s => Some((s, s + 1)))

  // For completeness, here are the constant methods
  // developed in InfiniteStreamTest.scala

  // My original version of constant
  def const1[A](a: A): Stream[A] =
    cons(a, const1(a))

  // More efficient since it is only
  // one object referencing itself.
  /** Take a value and return an infinite Stream of that value,
   *
   *  @param a the constant value to be returned by the Stream
   *  @return A Stream of constant values
   *  
   */
  def const[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Using unfold method for constant
  def constU[A](a: A): Stream[A] = 
    unfold(a) { _ => Some((a, a)) }

}
