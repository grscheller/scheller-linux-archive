package fpinscala.parallelism

/** ParUtils object.
 *
 *  Contains library functions that are less general
 *  than those in fpinscala.parallelism.Par companion
 *  object.  Basically, keeps the clutter down there,
 *  while giving me a place to experiment.
 *
 */
final object ParUtils {

  import fpinscala.parallelism.Par._

  /** Sum an IndexedSeq of par[Double] into a par of the sum. */
  def sumDoublesBalanced1(ps: IndexedSeq[Par[Double]]): Par[Double] =
    fork {
      if (ps.size == 1)
        ps(0)
      else {
        val (lps,rps) = ps.splitAt(ps.size/2)
        map2(sumDoublesBalanced1(lps), sumDoublesBalanced1(rps))(_ + _)
      }
    }

  /** Parallel calculation to sum an IndexedSeq[Double] */
  def sumDoublesParallel1(xs: IndexedSeq[Double]): Par[Double] =
    if (xs.isEmpty)
      unit(0.0)
    else
      sumDoublesBalanced1(xs.map(unit(_)))

  /** Parallel Calculation to find the maximum
   *  of an IndexedSeq of par[Int].
   */
  def maxIntsBalanced1(ps: IndexedSeq[Par[Int]]): Par[Int] =
    fork {
      if (ps.size == 1)
        ps(0)
      else {
        val (lps,rps) = ps.splitAt(ps.size/2)
        map2(maxIntsBalanced1(lps), maxIntsBalanced1(rps)) {
          (l, r) => if (l < r) r else l
        }
      }
    }

  /** Parallel calculation for the max of an IndexedSeq[Double] */
  def maxIntsParallel1(xs: IndexedSeq[Int]): Par[Int] =
    if (xs.isEmpty)
      throw new IllegalArgumentException("IndexedSeq must be non-empty")
    else
      maxIntsBalanced1(xs.map(unit(_)))

  // Abstract above
  //   During this process, I ended up rewritting above.
  def balancedParCalc[A](ps: IndexedSeq[Par[A]])(binOp: (A,A) => A): Par[A] =
    fork {
      if (ps.size == 1)
        ps(0)
      else {
        val (lps,rps) = ps.splitAt(ps.size/2)
        map2(balancedParCalc(lps)(binOp), balancedParCalc(rps)(binOp))(binOp)
      }
    }

  /** Parallel calculation to sum an IndexedSeq[Double] */
  def sumDoublesParallel(xs: IndexedSeq[Double]): Par[Double] =
    if (xs.isEmpty)
      unit(0.0)
    else
      balancedParCalc(xs.map(unit(_)))(_ + _)

  /** Parallel calculation to sum an IndexedSeq[Double] */
  def maxIntsParallel(xs: IndexedSeq[Int]): Par[Int] =
    if (xs.isEmpty)
      throw new IllegalArgumentException("IndexedSeq must be non-empty")
    else
      balancedParCalc(xs.map(unit(_))) { (l, r) => if (l < r) r else l }

}
