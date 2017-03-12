/** Package fpinscala.state.rand
 *
 *  Implemented the RNG and LCG classes.
 *
 *  Note: Rand is a type alias for
 *    fpinscala.state.State[RNG, A].  We define
 *    this package-wide in the rand package object.
 *  
 */
package fpinscala.state.rand

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
  def nonNegativeEven: Rand[Int] =
   nonNegativeInt.map(i => i - i%2)

  /** Generate a random Double between
   *  0 (inclusive) and 1 (exclusive).
   */
  def double: Rand[Double] = {
    val d = Int.MaxValue.toDouble + 1.0
    nonNegativeInt map { _.toDouble/d }
  }

  /** Random action non-negative Int less than n
   *
   *    1. Keeps things uniformly distributed over
   *       the range of the random variable even
   *       in the case n does not evenly divide
   *       the integer value Int.MaxValue + 1.
   *    2. For n = 0, you will get a divide by 0
   *       runtime java.lang.ArithmeticException.
   *    3. In "production code" I could replace
   *       n in the function body with n.abs,
   *       but this may mask a bug elsewhere.
   *       Better to range check if n <= 0 and
   *       throw some sort of range exception.
   *    4. Or, if performance is important and you
   *       are programming for people who are not
   *       idiots, have the client responsible
   *       for range checking.
   *
   *    Caller be warned, don't call with n <= 0.
   *
   */
  def nonNegativeLessThan(n: Int): Rand[Int] = 
    nonNegativeInt flatMap {
      (ii: Int) =>
        if (ii + (n-1) < 0)
          nonNegativeLessThan(n)
        else
          Rand.unit(ii % n)
    }

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
