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
  def sumDoublesBalanced(ps: IndexedSeq[Par[Double]]): Par[Double] =
    fork {
      if (ps.isEmpty)
        unit(0.0)
      else if (ps.length == 1)
        map(ps.head)(a => a)
      else {
        val (lps,rps) = ps.splitAt(ps.length/2)
        map2(sumDoublesBalanced(lps), sumDoublesBalanced(rps))(_ + _)
      }
    }

  /** Parallel calculation to sum an IndexedSeq[Double] */
  def sumDoublesParallel(xs: IndexedSeq[Double]): Par[Double] =
    sumDoublesBalanced(xs.map(unit(_)))

  /** Parallel Calculation to find the maximum
   *  of an IndexedSeq of par[Int].
   */
  def maxIntsBalanced(ps: IndexedSeq[Par[Int]]): Par[Int] =
    fork {
      val size = ps.size
      size match {
        case 1 => ps(0)
        case 2 => map2(ps(0), ps(1))(
                    (p0, p1) => if (p0 < p1) p1 else p0)
        case _ => { val (l,r) = ps.splitAt(size/2)
                    map2(maxIntsBalanced(l), maxIntsBalanced(r))(
                      (l, r) => if (l < r) r else l) }
      }
    }

  /** Parallel calculation for the max of an IndexedSeq[Double] */
  def maxIntsParallel(xs: IndexedSeq[Int]): Par[Int] =
    maxIntsBalanced(xs.map(unit(_)))

}
