package grockScala.laziness

/** Implement a lazy list */
sealed trait Stream[+A] extends Traversable[A] {

  import Stream._

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

  override def drop(n: Int): Stream[A] = 
    if (n < 1) this
    else this match {
      case Cons(_, t) => t().drop(n-1)
      case _ => Empty
    }

  override def take(n: Int): Stream[A] = 
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

  def dropWhile1(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => t().dropWhile1(p)
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

  override def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  override def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => 
      if (p(a)) cons(a, b)
      else Empty)

  override def dropWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => 
      if (p(a)) b
      else this)

  override def headOption: Option[A] = 
    foldRight(None: Option[A])((a, _) => Some(a))

  override def toList: List[A] =
    foldRight(Nil: List[A])((a, as) => a :: as.toList)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, bs) => cons(f(a), bs))

  override def filter(p: A => Boolean): Stream[A] =
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
    foldRight(bs)((a,as) => cons(a, as))

  /** Flatmap (Bind) for Streams */
  def flatMap1[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, bs) => f(a) #::: bs)  // Scala library way

  /** Flatmap (Bind) for Streams */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, bs) => f(a) :::# bs)  // Book ans way

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

}
