package fpinscala.state.rand

/** RNG base class for pseudo-random number generators. */
abstract class RNG {
  def nextInt: (Int, RNG)
  def nextRNG: (RNG, RNG) = nextInt match {
    case (int, rng) => (rng, rng)
  }
}

/** Extend RNG by Implementing the same Linear Congruence
  * Generator based pseudo-random number generating
  * algorithm used by java.util.Random and glibc.
  *
  *   newSeed = (a*seed + c) % m
  *
  *     where
  *       a = 25214903917 = 5DEECE66D
  *       c = 11 = B
  *       m = 2^48 = 281474976710656 = FFFFFFFFFFFF + 1
  *
  *   Pseudo-random int value = bits 47...16 of newSeed.
  *
  *   The higher order bits are less correlated than the
  *   lower order bits.  We shift by 16 bits to get a
  *   32-bit value.
  *
  *   A bit-& optimization is being used for the mod
  *   operator.  Basically we are (_ % 2^48) by ignoring
  *   all the digits of a long value past bit-47.
  *
  *   According to Knuth, you will get the maximum period
  *   of m, if and only if, the following conditions hold:
  *
  *     1. m and c are relatively prime,
  *     2. a-1 is divisible by all prime factors of m,
  *     3. a-1 is divisible by 4 if m is divisible by 4.
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
