/** A package whose purpose is used to
 *  1. Compare several trivial Fibonacci sequence implementations.
 *  2. Practice writing package level code off the top of my head.
 *  3. Reacquaint myself with Scala after doing Haskell for a while.
 *  4. Learn how to do generate nice docs from docstrings.
 */
package fibcompare

/** An imparertive implementation.
 *
 *  @note Use case would be if you just needed one value very fast.
 *  @note Use fibPair directly if you desired to bootstrap a
 *  subsequent calculation.
 */
object FibImperitive {

  /** Compute the nth Fibonacci number afresh each time in a tight loop */
  def fibN(n: Int, n0: BigInt = 0, n1: BigInt = 1 ): BigInt = 
    fibPair(n, n0, n1)._1

  /** Return a pair with the nth and (n+1)th Fibonacci numbers. */ 
  def fibPair(n: Int, n0: BigInt = 0, n1: BigInt = 1 ): (BigInt, BigInt) = {
    var a: BigInt = n0
    var b: BigInt = n1
    var c: BigInt = 0
    for (_ <- 1 to n) {
      c = a + b
      a = b
      b = c
    }
    (a, b)
  }

}

/** Hold onto a Stream of BigInt Fibonacci numbers
 *  @note Use case is to cache intermediate results in an object.
 */
case class FibCache(f0: BigInt = 0, f1: BigInt = 1) {
  val cachedFibs: Stream[BigInt] = f0 #:: f1 #:: {
    (cachedFibs zip cachedFibs.tail) map { case ((m, n)) => m + n }
  }

  def apply(n: Int): BigInt = cachedFibs(n)
} 

/** Name space for a method which returns stream of Fibonacci BigInts
 *
 *  @note Just hand the client the bloody cached Stream
 *  and let them deal with the Streams API.
 *
 *  @note Share fibs with the canonical start values 0, 1 
 */
object FibStream {

  /** Recursively, and lazily, build a stream of Fibonacci numbers. */
  def fibStream(a: BigInt = 0, b: BigInt = 1): Stream[BigInt] =
    a #:: fibStream(b, a+b)

  lazy val fibs: Stream[BigInt] = fibStream()

}
