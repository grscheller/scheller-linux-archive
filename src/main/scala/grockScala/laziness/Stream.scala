package grockScala.laziness

/** Implement a lazy list */
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())   // Explicitly evaluate the thunk
  }

  def tailOption: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(h, t) => Some(t())
  } 

  def tailSafe: Stream[A] = tailOption getOrElse (Stream.empty)

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def drop(n: Int): Stream[A] = 
    if (n < 1) this
    else this match {
      case Cons(h, t) => t().drop(n-1)
      case _ => Empty
    }

  def take(n: Int): Stream[A] = 
    if (n < 1) Empty
    else this match {
      case Cons(h, t) => Cons(h, () => t().take(n-1))
      case _ => Empty
    }

  def dropWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => t().dropWhile(p)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

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

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))

}
