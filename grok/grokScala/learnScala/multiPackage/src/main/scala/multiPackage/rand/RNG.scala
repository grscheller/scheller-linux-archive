/** Implemented the RNG and LCG classes
 *  for the package fpinscala.state.rand.
 *
 *  Note: Rand is a type alias for
 *        fpinscala.state.State[RNG, A].
 *        We define this package-wide in
 *        the rand package object.
 *  
 */
package grokScala.grok.rand

/** Base class for the RNG class for
 *  pseudo-random number generators.
 */
abstract class RNG {
  def nextInt: (Int, RNG)
}

/** Companion object for RNG class */
object RNG {

  /** State action to generate a random Int.
   *
   *  State((state: RNG) => (value: Int, nextState: RNG))
   *
   *  Aside: int(rng: RNG) is a random variable, in the sense
   *    of probability theory.  It is a function which maps
   *    values from some probability space (the possible 
   *    values of some subclass of RNG) to the space of
   *    32-bit signed integer values (Int).
   *
   *    For the LCG subclass, this mapping is uniform, i.e.
   *    it is equally likey to get any possible Int value.
   *
   *  Note: Have to use the "new" keyword otherwise the
   *    compiler thinks I am trying to use the apply
   *    method on the State companion object instead of
   *    the State[RNG, Int] case class constructor.
   *
   */
  def int: Rand[Int] = new Rand[Int](_.nextInt)

  /** Random action to generate a list of Int */
  def ints(count: Int): Rand[List[Int]] =
    Rand.sequence(List.fill(count)(int))

  /** Generate a random integer between
   *  0 and Int.maxValue (inclusive).
   */
  def nonNegativeInt: Rand[Int] = new Rand(rng =>
    rng.nextInt match {
      case (ran, rng2) if ran >= 0            => ( ran, rng2)
      case (ran, rng2) if ran == Int.MinValue => (   0, rng2)
      case (ran, rng2)                        => (-ran, rng2)
    })

  /** Generate an even random integer between
   *  0 and Int.maxValue (inclusive).
   */
  def nonNegativeEvenInt: Rand[Int] =
   nonNegativeInt map { ii => ii - ii%2 }

  /** Random action non-negative Int less than n
   *
   *    1. Keeps things uniformly distributed over
   *       the range of the random variable even
   *       in the case n does not evenly divide
   *       the integer value Int.MaxValue + 1.
   *    2. Algorithm assumes that n > 0.
   *    3. Stack overflow can happen if n < 0.
   *    4. For n = 0, you will get a divide by 0
   *       runtime java.lang.ArithmeticException.
   *    5. For efficiency, client responsible to
   *       ensure that n > 0.
   *
   *    Caller be warned, don't call with n <= 0.
   *
   */
  def nonNegativeIntLessThan(n: Int): Rand[Int] = 
    nonNegativeInt flatMap { ii =>
      val mod = ii % n
      if (ii + ((n-1) - mod) >= 0)
        Rand.unit(mod)
      else
        nonNegativeIntLessThan(n)
    }

  /** Random Int within the range start <= random_variable < end
   *
   *    Pathological cases:
   *      If start = end, always generate start.
   *      If statt > end, generate start >= randome_variable > end
   *
   *    Works great unless (end - start) > Int.Maxvalue, then things
   *    no longer transparently simple.  If only java had unsigned types!
   *
   */
  def exclusiveIntRange(start: Int, end: Int): Rand[Int] = {
    val len = if (start != end) end - start else 1
    val sign = len/len.abs
    nonNegativeIntLessThan(len.abs) map {
      (ii: Int) => start + sign*ii
    }
  }

  /** Generate a random Double between
   *  0 (inclusive) and 1 (exclusive).
   */
  def double: Rand[Double] = {
    val d = Int.MaxValue.toDouble + 1.0
    nonNegativeInt map { _.toDouble/d }
  }

  /** Generate a random boolean */
  def boolean: Rand[Boolean] =
    int map {ii => ii % 2 == 0}

}

/** Extend RNG by Implementing the same Linear Congruence
 *  Generator based pseudo-random number generating
 *  algorithm used by java.util.Random and glibc.
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
