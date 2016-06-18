package grockScala.laziness

/** Implement a lazy list
 *
 * @author    "Geoffrey Scheller <geoffrey@scheller.com>"
 * @version   1.0
 * @since     1.0
 */
sealed trait Stream[+A] {

  import Stream._

  /** For for expressions
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
   *  filter method for guards in for comprehensions/expresions
   *  but complain about it. Filter creates an intermediate data
   *  structure.  An actual withFilter method wraps the monadic
   *  object in a withFilter object with flatMap, map, and foreach
   *  methods which lazily filters out the undesired elements.
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
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList1
  }

  def drop(n: Int): Stream[A] = 
    if (n < 1) this
    else this match {
      case Cons(_, t) => t().drop(n-1)
      case _ => Empty
    }

  def take(n: Int): Stream[A] = 
    if (n < 1) Empty
    else this match {
      case Cons(h, t) => Cons(h, () => t().take(n-1))
      case _ => Empty
    }

  def takeWhile1(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile1(p))
      case _ => Empty
    }

  def dropWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => t().dropWhile(p)
      case _ => this
    }

  def exists1(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists1(p)
      case _ => false
    }

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
    foldRight(empty[B])((a, bs) => f(a) :::# bs)  // Book ans way

  /** Return the first element which matches a predicate */
  def find(p: A => Boolean) =
    filter(p).headOption

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
   *  
   */
  def unfold[A,S](s: S)(f: S => Option[(A, S)]): Stream[A] =
    f(s) flatMap (p => Some(cons(p._1, unfold(p._2)(f)))) getOrElse empty

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

}
