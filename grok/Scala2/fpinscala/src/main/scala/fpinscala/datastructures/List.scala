package fpinscala.datastructures

/** Reimplement Scala's List type */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /** A total function version where tail(Nil) == Nil */
  def tailT[A](l: List[A]): List[A] =
    l match {
      case Cons(h, t) => t
      case _ => l
    }

  /** Return first element of List - partial function
   *
   *  @note Well known that head of an empty list fails.
   *  @note Unchecked match used to gag compiler warning.
   *
   */
  def head[A](l: List[A]): A =
    (l: @unchecked) match {
      case Cons(h, t) => h
    }

  /** Return a list with a replaced first element */
  def setHead[A](l: List[A], a: A): List[A] =
    l match {
      case Cons(h, t) => Cons(a, t)
      case _ => l
    }

  /** Drop n elements from beginning of list */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1)
      l
    else
      l match {
        case Cons(h, t) => drop(t, n-1)
        case _ => l
      }

  /** Drop elements from beginning of list while condition true */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) =>
        if (f(h))
          dropWhile(t, f)
        else
          l
      case _ => l
    }

  /** Version of dropWhile using a guard */
  def dropWhile1[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if (f(h)) => dropWhile1(t, f)
      case _ => l
    }

  /** Curried version of dropWhile1 - gives improved type inference */
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if (f(h)) => dropWhile2(t)(f)
      case _ => l
    }

  // Exercise 3.6
  /** Remove last element in list - not a total function since
   *  the empty list has no last element to remove.
   *
   *  @note Well known that init is not a total function.
   *  @note Unchecked match used to gag compiler warning.
   *  @note Sensitive to stackoverflow.
   */
  def init[A](l: List[A]): List[A] =
    (l: @unchecked) match {
      case Cons(h1, Cons(h2, Nil)) => Cons(h1, Nil)
      case Cons(h1, Cons(h2, rest)) => Cons(h1, Cons(h2, init(rest)))
      case Cons(h, Nil) => Nil
    }

  /** Product of a list of doubles */
  def productL1(xs: List[Double]) = foldLeft(xs, 1.0)(_ * _)

  /** Product of a list of doubles */
  def productR1(xs: List[Double]) = foldRightUnsafe(xs, 1.0)(_ * _)

  // Exercise 3.7
  /** Right fold with short circuit logic
   *
   *  @note Names zero and one based on analogy to a multiplicative group.
   *
   */
  def foldRightSC[A,B](xs: List[A], zero: A, one: B)(f: (A, B) => B): B =
    xs match {
      case Nil => one
      case Cons(x, rest) if x == zero => f(zero, one)
      case Cons(x, rest) => f(x, foldRightSC(rest, zero, one)(f))
    }

  /** Product of a list of doubles, does not bother processing
   *  the rest of list if 0.0 is encountered
   */
  def productSC(xs: List[Double]) = foldRightSC(xs, 0.0, 1.0)(_ * _)

  /** Concatenate strings in List until an empty String is encountered */
  def catSC(xs: List[String]) = foldRightSC(xs, "", "")(_ ++ _)

  // Exercise 3.9
  /** Find length of List */
  def length[A](as: List[A]): Int = {
    def lenAcc(n: Int)(l: List[A]): Int =
      l match {
        case Cons(h, rest) => lenAcc(n+1)(rest)
        case _ => n
    }

    lenAcc(0)(as)
  }

  /** Implement foldRight - not tail recursive, not stack safe */
  def foldRightUnsafe[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRightUnsafe(xs, z)(f))
    }

  // Exercise 3.10 - Implement tail recursive foldLeft
  /** Tail recursive foldLeft - stack safe */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def acc(lt: B, rt: List[A]): B =
      rt match {
        case Nil => lt
        case Cons(x, xs) => acc(f(lt, x), xs)
      }

    acc(z, as)
  }

  /** Tail recursive foldLeft - with Short Circuit */
  def foldLeftSC[A, B](as: List[A], zero: A, one: B)(f: (B, A) => B): B = {
    def acc(lt: B, rt: List[A]): B =
      rt match {
        case Nil => lt
        case Cons(x, xs) if x == zero => f(one, zero)
        case Cons(x, xs) => acc(f(lt, x), xs)
      }

    acc(one, as)
  }

  // Exercise 3.11 (Partial)
  /** Length of List via foldLeft */
  def lengthL[A](as: List[A]): Int =
    foldLeft(as, 0)( (n, _) => n + 1 )

  // Exercise 3.12
  /** Reverse the elements of a List */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((as, a) => Cons(a, as))

  // Exercise 3.13 - Implement foldRight with foldLefts
  /** Tail recursive foldRight - stack safe */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(foldLeft(as, Nil: List[A])((as, a) => Cons(a, as)), z)((b, a) => f(a, b))

  // Exercise 3.14 - Implement append via either foldsLeft or foldRight
  /** Append 2nd List to First */
  def append[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)(Cons(_, _))

  // Exercise 3.15 - Implement flatten linearly in length of the list
  /** Flatten a List of Lists */
  def flatten[A](ass: List[List[A]]): List[A] =
    foldRight(ass, Nil: List[A])(append)

  // Exercise 3.16
  /** Transform a List[Int] by incrementing each element by 1 */
  def bump1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, as) => Cons(a + 1, as))

  // Exercise 3.17
  /** Transform a List[Double] to a List[String] */
  def doublesToStrings(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, ds) => Cons(d.toString, ds))

  // Exercise 3.18
  /** Modify each element of a list while maintaining
   *  while maintaining the structure of the List
   */
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, bs) => Cons(f(a), bs))

  // Exercise 3.19
  /** Filter elements from a list based on a predicate */
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])(
      (a, as) =>
        if (f(a))
          Cons(a, as)
        else
          as
    )

  // Exercise 3.20
  /** Implement flatMap for List */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  // Exercise 3.21
  /** Reimplimention of filter using flatMap */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(
      a =>
        if (f(a))
          List(a)
        else
          Nil
    )

  // Exercise 3.22
  /** Add corressponding elements of 2 lists of Int's - match up from head */
  def addLists(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Cons(a, rest_as), Cons(b, rest_bs)) =>
        Cons(a+b, addLists(rest_as, rest_bs))
      case _ =>
        Nil
    }

  // Exercise 3.23
  /**
   *  Implements standard zipWith function
   *
   *  @note Not stack safe
   */
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
    (as, bs) match {
      case (Cons(a, rest_as), Cons(b, rest_bs)) =>
        Cons(f(a, b), zipWith(rest_as, rest_bs)(f))
      case _ =>
        Nil
    }

  /*
   *  Implements standard zipWith function
   *
   *  Todo: when I know how to make things lazy
   */
  //def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
  //  foldLeft(as, Nil: List[C])

  // Exercise 3.24
  /** Determine if a List contains another List as a subsequence */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def hasHead(h: A, l: List[A]): Boolean =
      l match {
        case Cons(h2, rest) if h == h2 => true
        case _ => false
      }

    def sameInit(l1: List[A], l2: List[A]): Boolean  =
      foldLeftSC(zipWith(l1, l2)(_ == _), false, true)(_ && _)

    def foundIt(l: List[A]): Boolean =
      if (hasHead(head(sub), l))
        if (sameInit(l, sub))
          true
        else
          foundIt(tailT(l))
      else
        if (l == Nil)
          false
        else
          foundIt(tailT(l))

    if (sub != Nil)
      foundIt(sup)
    else
      true

  }

}
