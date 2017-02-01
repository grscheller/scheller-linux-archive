package grockScala.state

/** Trait for pseudo-random number generators
 */
trait RNG {
  def nextInt: (Int,RNG)
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

  private val a = 0x5DEECE66DL
  private val c = 0xBL
  private val modMask = 0xFFFFFFFFFFFFL

  def nextInt: (Int,RNG) = {
    val newSeed = (a*seed + c) & modMask
    val nextRNG = LCG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

/** Object to provide namespace for RNG trait utility functions.  */
object RNG {

  /** Generate a random integer between
   *  0 and Int.maxValue (inclusive).
   */
  def nonNegativeInt(rng: RNG): (Int,RNG) =
    rng.nextInt match {
      case (ran, rng2) if ran >= 0            => ( ran, rng2)
      case (ran, rng2) if ran == Int.MinValue => (   0, rng2)
      case (ran, rng2)                        => (-ran, rng2)
    }

  // Book's version nonNegativeInt
  //   As with mine above, there are precisely two ways
  //   to get any particular non-negative integer.  This
  //   way everything stays equally weighted.
  def nonNegativeInt1(rng: RNG): (Int,RNG) = {
    val (ran, rng2) = rng.nextInt
    (if (ran < 0) -(ran + 1) else ran, rng2)
  }

  /** Generate a random double between
   *  0 (inclusive) and 1 (exclusive).
   *
   *  Initial version.
   */
  def double1(rng: RNG): (Double,RNG) =
    nonNegativeInt(rng) match {
      case (ranNNI, rng1) =>
        ( ranNNI.toDouble/(Int.MaxValue.toDouble + 1.0), rng1 )
    }

  def intDouble(rng: RNG): ((Int,Double),RNG) = {
    val (ranI, rng1) = rng.nextInt
    val (ranD, rng2) = double(rng1)
    ((ranI, ranD), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int),RNG) =
    intDouble(rng) match {
      case ((int, doub), rng1) => ((doub, int), rng1)
    }

  // Book's version
  def doubleInt1(rng: RNG): ((Double,Int),RNG) = {
    val ((int, doub), rng1) = intDouble(rng)
    ((doub, int), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double),RNG) = {
    val (doub1, rng1) = double(rng)
    val (doub2, rng2) = double(rng1)
    val (doub3, rng3) = double(rng2)
    ((doub1, doub2, doub3), rng3)
  }

  def ints1(count: Int)(rng: RNG): (List[Int], RNG) =
    count match {
      case n if n <= 0 => (Nil, rng)
      case n           => {
        val (ii, rng1) = rng.nextInt
        val (l, rng2) = ints(n-1)(rng1)
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
  type Rand[+A] = RNG => (A,RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng1) = s(rng)
      (f(a), rng1)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i%2)

  /** Generate a random Double between
   *  0 (inclusive) and 1 (exclusive).
   *
   *    (state: RNG) => (value, nextState)
   *
   *  Random variable in the sense of probability theory.
   */
  def double: Rand[Double] = {
    val d = Int.MaxValue.toDouble + 1.0
    map(nonNegativeInt)(_.toDouble/d)
  }

  /** Generate a random Int
   *
   *    (state: RNG) => (value: Int, nextState: RNG)
   *
   *  Random variable in the sense of probability theory.
   */
  def int: Rand[Int] = _.nextInt

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int,Double)] = both(int, double)

  def randDoubleInt: Rand[(Double,Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    if (fs.isEmpty)
      unit(List())
    else
      rng => {
        val (f, rngH) = fs.head(rng)
        val (fTail, rngT) = sequence(fs.tail)(rngH)
        (f :: fTail, rngT)
      }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)((_: RNG).nextInt))(rng)

}
