package grockScala.errorhandling

/** Implement Option ADT to handle error conditons */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
    
/*
  /** Apply f, which may fail, to the Option, if not none */
  def flatMap[B](f: A => Option[B]): Option[B]

  /** Return value, if None return default, default nonstrict */
  def getOrElse[B >: A](default: => B): B

  /** If None, swap with superclass Option, nonstrictly */
  def orElse[B >: A](default: => Option[B]): Option[B]

  /** Convert Some to None if predicate false */
  def filter(pred: A => Boolean): Option[A]
*/
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

