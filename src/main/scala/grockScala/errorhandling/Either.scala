package grockScala.errorhandling

/**
 * Implement Either ADT to handle error conditions.
 *   In some respects, this data structure has some similarity
 *   to a union in the C programming language.  An area in memory
 *   can be one thing or another, but not both.  A crucial
 *   difference is that pattern matching allows us to tell which
 *   one it is at run time.
 */
sealed trait Either[+E, +A] {

  /** Apply function to RHS value of Either, if not None */
  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  /** Apply f, which may fail, to RHS of Either, if not None */
  def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  /** Return value if Right, otherwise return default, default nonstrict */
  def getOrElse[B >: A](default: => B): B = this match {
    case Left(_) => default
    case Right(a) => a
  }

  /**
   *  If Left, nonstrictly swap with superclass Either, either
   *  a Left or Right will work.
   */
  def orElse[EE >: E,B >: A](default: => Either[EE,B]): Either[EE,B] =
    this match {
      case Left(_) => default
      case _ => this
    }

  def map2[EE >: E,B,C](bE: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    this.flatMap(a => bE map (b => f(a, b)))

  def map2_for[EE >: E,B,C](bE: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    for {
      a <- this
      b <- bE
    } yield f(a, b)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  /**
   *  General purpose function that converts from exception
   *  based APIs to an Either oriented one.
   */
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

//  // In real life I would probably just use the map directly.
//  /** Take a fucntion and "lift" it to work on options */
//  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

//  def lift2[A,B,C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] =
//    map2r(f)(_, _)

  /**
   *  Take a list, apply a function which returns a Either
   *  to each element of the list and if all are Rights
   *  return a Either of a list of all the values in the
   *  Rights, otherwise return a None.
   */
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    as.foldRight(Right(Nil): Either[E,List[B]])(
      (a, bsO) => 
        for {
          bs <- bsO
          b <- f(a)
        } yield b :: bs )

  /**
   *  Take a List of Options, if all are Some, return an Option of a
   *  a single List of the Option values, otherwise return None.
   */
  def sequence[E,A](aEs: List[Either[E,A]]): Either[E,List[A]] =
    traverse(aEs)((aE: Either[E,A]) => aE)

}
