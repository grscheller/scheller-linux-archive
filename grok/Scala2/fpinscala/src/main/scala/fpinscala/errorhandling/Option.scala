package fpinscala.errorhandling

/** Implement Option ADT to handle error conditions */
sealed trait Option[+A] {

  /** Apply function to value in Option, if not None */
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None    => None
  }

  /** Apply f, which may fail, to the Option, if not None */
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None    => None
  }

  /** Return value, if None return default, default nonstrict */
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None    => default
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

  // Some extra stuff

  /** Fold left, could be more useful with some type variance */
  def foldLeft[B](z: B)(f: (B, A) => B): Option[B] =
    this map ((a: A) => f(z, a))

  /** Fold right, could be more useful with some type variance */
  def foldRight[B](z: B)(f: (A, B) => B): Option[B] =
    this map ((a: A) => f(a, z))

  /**  Flatten an Option of an Option to a single Option.
    *
    *  @note This is tricky, type errasure in scala prevents
    *  deep pattern matching.  I looked up how this was
    *  done in the Scala standard library for the Option
    *  abstract class.  The sematics is that type A is required
    *  to be a subtype of an Option of an A supertype, hence
    *  an option itself.  Current implementation works, but not
    *  sure what syntaxical is meant by the syntax.
    */
  def flatten[B](implicit ev: A <:< Option[B]): Option[B] =
    this match {
      case Some(oA) => oA
      case _        => None
    }

  /** Apply f, which may fail, to the Option, if not None */
  def flatMap2[B](f: A => Option[B]): Option[B] =
    (this map f).flatten

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

/** Utility functions for working with Options.
  */
object Option {

  /**  General purpose function that converts from exception
    *  based APIs to an Option oriented one.
    */
  def Try[A](a: => A): Option[A] = // Lazy evaluation so argument
    try Some(a) // is evaluated in the try block.
    catch { case e: Exception => None }

  // In real life I would probably just use the map directly.
  /** Take a fucntion and "lift" it to work on options */
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  /**  Take two Options and a function of two arguments, with no
    *  knowledge of Options, apply the function with the values
    *  within the the Options and return an Option containing
    *  the result.
    */
  def map2_mine[A, B, C](aO: Option[A], bO: Option[B])(
      f: (A, B) => C
  ): Option[C] =
    aO.flatMap(a => bO.flatMap(b => Some(f(a, b)): Option[C]))

  // Book's version of map2.  At first I like mine better except
  // for having to type annotate the above Some.
  //
  //   After a while, I see why this one is better.  The second
  //   flatMap is providing me no protection for f failing (by
  //   throwing exceptions) but still has the overhead to explicitly
  //   check that the Some is not a None.
  //
  /**  Take two Options and a function of two arguments, with no
    *  knowledge of Options, apply the function with the values
    *  within the the Options and return an Option containing
    *  the result.
    */
  def map2[A, B, C](aO: Option[A], bO: Option[B])(f: (A, B) => C): Option[C] =
    aO.flatMap(a => bO map (b => f(a, b)))

  /**  Similar to Option.map2 except arguments reversed.
    *
    *  Now, when partially applied with a function of two arguments,
    *  with no knowledge of Options, a function that operates with
    *  the corresponding Options and returning an Option.
    *
    *  Of course this trick would not have worked if we had put this
    *  method in the trait itself instead of the companion object.
    */
  def map2r[A, B, C](f: (A, B) => C)(aO: Option[A], bO: Option[B]): Option[C] =
    aO.flatMap(a => bO map (b => f(a, b)))

  /* Lift a function to the Option monad */
  def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] =
    map2r(f)(_, _)

  /**  Take a List of Options, if all are Some, return an Option of a
    *  a single List of the Option values, otherwise return None.
    */
  def sequence1[A](aOs: List[Option[A]]): Option[List[A]] =
    aOs.foldRight(Some(Nil): Option[List[A]])((aO, asO) =>
      aO.flatMap(a => asO.map(as => a :: as))
    )

  // Above translated into for comprehension
  def sequence2[A](aOs: List[Option[A]]): Option[List[A]] =
    aOs.foldRight(Some(Nil): Option[List[A]])((aO, asO) =>
      for {
        a <- aO
        as <- asO
      } yield a :: as
    )

  // Trivial change
  def sequence3[A](aOs: List[Option[A]]): Option[List[A]] =
    aOs.foldRight(Some(Nil): Option[List[A]])((aO, asO) =>
      for {
        as <- asO
        a <- aO
      } yield a :: as
    )

  // Translate above back into explicitly functional notation
  //   If I were smarter, this is the one I would have come up
  //   with first.  I think it is more efficient, once the built
  //   up array becomes a None, everything shorts out without
  //   ever examining another Option[a].
  def sequence4[A](aOs: List[Option[A]]): Option[List[A]] =
    aOs.foldRight(Some(Nil): Option[List[A]])((aO, asO) =>
      asO.flatMap(as => aO.map(a => a :: as))
    )

  /**  Take a list, apply a function which returns an Option
    *  to each element of the list and if none are None,
    *  return an Option of a list of all the values in the
    *  Somes, otherwise return a None.
    */
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((a, bsO) =>
      for {
        bs <- bsO
        b <- f(a)
      } yield b :: bs
    )

  // Direct translation
  def traverse1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((a, bsO) =>
      bsO.flatMap(bs => f(a).map(b => b :: bs))
    )

  // Simplification using sections
  def traverse2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil): Option[List[B]])((a, bsO) =>
      bsO.flatMap(bs => f(a).map(_ :: bs))
    )

  /**  Take a List of Options, if all are Some, return an Option of a
    *  a single List of the Option values, otherwise return None.
    *
    *  @param aOs a list of Options of type A.
    *  @return A single Option of a List of type A.
    *  @note All or nothing.
    */
  def sequence[A](aOs: List[Option[A]]): Option[List[A]] =
    traverse(aOs)((aO: Option[A]) => aO)

}
