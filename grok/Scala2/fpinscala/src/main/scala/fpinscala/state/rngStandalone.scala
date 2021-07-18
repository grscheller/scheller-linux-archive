package fpinscala.rngStandalone

// I have split this off to its own standalone implementation.
// I will define a State case class in another file.  I will
// reimplent what was done here with it.

/** Trait for pseudo-random number generators
  */
trait RNG {
  def nextInt: (Int, RNG)
}

/** Implement the same Linear Congruence Generator
  *  based pseudo-random number generating algorithm
  *  used by java.util.Random and glibc.
  *
  *    newSeed = (a*seed + c) % m
  *
  *      where
  *        a = 25214903917 = 5DEECE66D
  *        c = 11 = B
  *        m = 2^48 = 281474976710656 = FFFFFFFFFFFF + 1
  *
  *    Pseudo-random int value = bits 47...16 of newSeed.
  *
  *    The higher order bits are less correlated than the
  *    lower order bits.  We shift by 16 bits to get a
  *    32-bit value.
  *
  *    A bit-& optimization is being used for the mod
  *    operator.  Basically we are (_ % 2^48) by ignoring
  *    all the digits of a long value past bit-47.
  *
  *    According to Knuth, you will get the maximum period
  *    of m, if and only if, the following conditions hold:
  *
  *      1. m and c are relatively prime,
  *      2. a-1 is divisible by all prime factors of m,
  *      3. a-1 is divisible by 4 if m is divisible by 4.
  */
case class LCG(seed: Long) extends RNG {

  private val a = 0x5deece66dL
  private val c = 0xbL
  private val modMask = 0xffffffffffffL

  def nextInt: (Int, RNG) = {
    val newSeed = (a * seed + c) & modMask
    val nextRNG = LCG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

/** Object to provide namespace for RNG trait utility functions. */
object RNG {

  /** Generate a random integer between
    *  0 and Int.maxValue (inclusive).
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (ran, rng2) if ran >= 0            => (ran, rng2)
      case (ran, rng2) if ran == Int.MinValue => (0, rng2)
      case (ran, rng2)                        => (-ran, rng2)
    }

  // Book's version nonNegativeInt
  //   As with mine above, there are precisely two ways
  //   to get any particular non-negative integer.
  //   This keeps everything equally weighted.
  def nonNegativeInt1(rng: RNG): (Int, RNG) = {
    val (ran, rng2) = rng.nextInt
    (if (ran < 0) -(ran + 1) else ran, rng2)
  }

  /** Generate a random double between
    *  0 (inclusive) and 1 (exclusive).
    *
    *  Initial version.
    */
  def double1(rng: RNG): (Double, RNG) =
    nonNegativeInt(rng) match {
      case (ranNNI, rng1) =>
        (ranNNI.toDouble / (Int.MaxValue.toDouble + 1.0), rng1)
    }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (ranI, rng1) = rng.nextInt
    val (ranD, rng2) = double(rng1)
    ((ranI, ranD), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    intDouble(rng) match {
      case ((int, doub), rng1) => ((doub, int), rng1)
    }

  // Book's version
  def doubleInt1(rng: RNG): ((Double, Int), RNG) = {
    val ((int, doub), rng1) = intDouble(rng)
    ((doub, int), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doub1, rng1) = double(rng)
    val (doub2, rng2) = double(rng1)
    val (doub3, rng3) = double(rng2)
    ((doub1, doub2, doub3), rng3)
  }

  def ints1(count: Int)(rng: RNG): (List[Int], RNG) =
    count match {
      case n if n <= 0 => (Nil, rng)
      case n => {
        val (ii, rng1) = rng.nextInt
        val (l, rng2) = ints1(n - 1)(rng1)
        (ii :: l, rng2)
      }
    }

  /* Note that types cannot be defined outside
   * of classes/objects.  They are a feature that
   * is part of Scala's OO system - see page 457 of
   * Oderski's Programming in Scala, 3rd edition,
   * on path dependent types.  Types are members
   * just like defs, vals, and vars.  FPinScala is
   * using the feature here to implement type aliases.
   */
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng1) = s(rng)
      (f(a), rng1)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /** Generate a random Double between
    *  0 (inclusive) and 1 (exclusive).
    *
    *    (state: RNG) => (value, nextState)
    *
    *  Random variable in the sense of probability theory.
    */
  def double: Rand[Double] = {
    val d = Int.MaxValue.toDouble + 1.0
    map(nonNegativeInt)(_.toDouble / d)
  }

  /** Generate a random Int
    *
    *    (state: RNG) => (value: Int, nextState: RNG)
    *
    *  Random variable in the sense of probability theory.
    */
  def int: Rand[Int] = _.nextInt

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb) { (_, _) }

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /** Combine a list of random actions into one action.
    *
    *    My initial version.
    */
  def sequenceRecursion[A](fs: List[Rand[A]]): Rand[List[A]] =
    if (fs.isEmpty)
      unit(List())
    else
      rng => {
        val (f, rngH) = fs.head(rng)
        val (fTail, rngT) = sequenceRecursion(fs.tail)(rngH)
        (f :: fTail, rngT)
      }

  /** Combine a list of random actions into one action.
    *
    *    Book's inital version - using foldRight:
    */
  def sequenceFR[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) { (f, acc) =>
      map2(f, acc)(_ :: _)
    }

  /** Combine a list of random actions into one action.
    *
    *    My first attempt at Book's suggested improved version.
    *    Gives resulting List reversed.  I don't know why I
    *    "clearly saw" the result would "obviously" need to
    *    be reversed.
    *
    *    Originally kept it around because I found
    *    the reversing step interesting.  Turns out the reversing
    *    does keep the the resulting random List in the same
    *    order as the list of random actions.  What gets reversed
    *    is the evaluation order.
    *
    *    My original intuition was correct.
    */
  def sequenceFLRev[A](fs: List[Rand[A]]): Rand[List[A]] =
    RNG.map(fs.foldLeft(unit(List[A]()))((acc, f) => map2(f, acc)(_ :: _))) {
      _.reverse
    }

  /** Combine a list of random actions into one action.
    *
    *    Reverse list first, then do left fold.
    *
    *    This keeps results the same as simple recursion.
    *
    *    Fold Lefts for (strict) scala lists are more stacksafe
    *    and efficient than fold rights, I beleive this to be
    *    best implemetation.
    */
  def sequenceRevFL[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.reverse.foldLeft(unit(List[A]()))((acc, f) => map2(f, acc)(_ :: _))

  /** Combine a list of random actions into one action.
    *
    *    Book's suggested improved version - using foldLeft:
    *      I think this one is O(n)?
    */
  def sequenceFL[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List[A]())) { (acc, f) =>
      map2(f, acc)(_ :: _)
    }

  /** Combine a list of random actions into one action.
    *
    *    Choosing the sequenceRevFL implementation.
    */
  def sequence[A]: List[Rand[A]] => Rand[List[A]] = sequenceRevFL

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  /** Random action non-negative Int less than n
    *
    *    Problems:
    *    1. If n does not evenly divide the integer
    *       value Int.MaxValue + 1, the result will
    *       not be uniformly distributed over the
    *       range of the random veriable.
    *    2. For n = 0, you will get a divide by 0
    *       runtime java.lang.ArithmeticException.
    *    3. In "production code" I could replace
    *       n in the function body with n.abs,
    *       but this may mask a bug elsewhere.
    *       Better to range check if n <= 0 and
    *       throw some sort of range exception.
    */
  def nonNegativeLessThanNonUniform(n: Int): Rand[Int] =
    map(nonNegativeInt) { _ % n }

  /** Random action non-negative Int less than n
    *
    *    It would have been more "natural" to implement
    *    this in terms of unsigned Int types, but
    *    unfortunately the JVM does not have them.
    *
    *    Caller be warned, don't call with n <= 0.
    */
  def nonNegativeLessThanManual(n: Int): Rand[Int] =
    rng1 => {
      val (ii, rng2) = nonNegativeInt(rng1)
      if (ii + (n - 1) < 0)
        nonNegativeLessThanManual(n)(rng2)
      else
        (ii % n, rng2)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { ii =>
      val mod = ii % n
      if (ii + ((n - 1) - mod) >= 0) unit(mod) else nonNegativeLessThan(n)
    }

// Implement map and map2 in terms of flatMap
//   Essentially idetical with book, maybe I took
//   "implement in terms of flatMap" too literally.

  def map_[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r) { a => unit(f(a)) }

  def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a => flatMap(rb) { b => unit(f(a, b)) } }

  def map_book[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2_book[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map_book(rb)(b => f(a, b)))

}
