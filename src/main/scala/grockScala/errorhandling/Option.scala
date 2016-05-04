package grockScala.errorhandling

/** Implement Option ADT to handle error conditons */
sealed trait Option[+A] {

  /** Apply function to value in Option, if not None */
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  /** Apply f, which may fail, to the Option, if not None */
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  /** Return value, if None return default, default nonstrict */
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  /** If None, swap with superclass Option, nonstrictly */
  def orElse[B >: A](default: => Option[B]): Option[B] =
    if (this == None) default
    else this

  /** Convert Some to None if predicate false */
  def filter(pred: A => Boolean): Option[A] = 
    flatMap((a: A) =>
      if (pred(a)) this
      else None
    )

// Versions from book answers
  /** Apply f, which may fail, to the Option, if not none */
  def flatMap_book[B](f: A => Option[B]): Option[B] = 
    map(f).getOrElse(None)

  /** If None, swap with superclass Option, nonstrictly */
  def orElse_book[B >: A](default: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(default)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  /**
   *  General purpose function that converts from exception
   *  based APIs to an Option oriented one.
   */
  def Try[A](a: => A): Option[A] =      // Lazy evaluation so argument
    try Some(a)                         // is evaluated in the try block.
    catch { case e: Exception => None }

  // In real life I would probably just use the map directly.
  /** Take a fucntion and "lift" it to work on options */
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  /**
   *  Take two Options and a function of two arguments, with no
   *  knowledge of Options, apply the function with the values 
   *  within the the Options and return on Option containing
   *  the result.
   */
  def map2_mine[A,B,C](aO: Option[A],
                       bO: Option[B])(f: (A, B) => C): Option[C] =
    aO.flatMap(a => bO.flatMap(b => Some(f(a, b)): Option[C]))

  // Book's version of map2.  I like mine better except for having
  // to type annotate the above Some.
  //
  //   After a while, I see why this one is better.  The second
  //   flatMap is providing me no protection for f failing (by
  //   throwing exceptions) but still has the overhead to explicitly
  //   check that the Some is not a None.
  //
  /**
   *  Take two Options and a function of two arguments, with no
   *  knowledge of Options, apply the function with the values 
   *  within the the Options and return on Option containing
   *  the result.
   */
  def map2[A,B,C](aO: Option[A], bO: Option[B])(f: (A, B) => C): Option[C] =
    aO.flatMap(a => bO map (b => f(a, b)))

  /**
   *  Similar to Option.map2 except arguments reversed.
   *
   *  Now, when partially applied with a function of two arguments,
   *  with no knowledge of Options, a function that operates with
   *  the corresponding Options and returning an Option will be returned.
   */
  def map2r[A,B,C](f: (A, B) => C)(aO: Option[A], bO: Option[B]): Option[C] =
    aO.flatMap(a => bO map (b => f(a, b)))

  def lift2[A,B,C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] =
    map2r(f)(_, _)

  /**
   *  Take a List of Options, if all are Some, return an Option of a
   *  a single List of the Option values, otherwise return None.
   */
  def sequence1[A](aOs: List[Option[A]]): Option[List[A]] =
    aOs.foldRight(Some(Nil): Option[List[A]])((aO, asO) =>
      aO.flatMap(a => asO.flatMap(as => Some(a :: as)))
    )

  // Above translated into for comprehension
  def sequence2[A](aOs: List[Option[A]]): Option[List[A]] =
    aOs.foldRight(Some(Nil): Option[List[A]])((aO, asO) =>
      for {
        a <- aO
        as <- asO
      } yield a :: as
    )

}
