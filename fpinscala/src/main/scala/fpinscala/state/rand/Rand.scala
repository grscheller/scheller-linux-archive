package fpinscala.state.rand

import fpinscala.state.State

/** Rand case class
 *
 */
case class Rand[+A](action: State[RNG,A]) {

  def flatMap[B](g: A => Rand[B]): Rand[B] =
    Rand(action.flatMap(a => g(a).action))

  def map[B](f: A => B): Rand[B] =
    Rand(action.map(f))

  def map2[B,C](rv: Rand[B])(f: (A,B) => C): Rand[C] =
    Rand(action.map2(rv.action)(f))

  def apply(s: RNG): A = action.run(s)._1

}

object Rand {

  def unit[A](a: A): Rand[A] = Rand(State(s => (a, s)))

  /** A simple version of sequence - stackoverflow cranky */
  def sequenceSimple[A](rvs: List[Rand[A]]): Rand[List[A]] =
    rvs.reverse.foldLeft(unit[List[A]](Nil)) {
      (acc, rv) => rv.map2(acc)(_ :: _)
    }

  /** Combine an indexable collection of random variables into
   *  a random variable via a binary operator.
   *
   *    Note: For efficiency, collection assumed nonempty.
   */
  def balancedBinComp[A](rands: IndexedSeq[Rand[A]])
                        (binOp: (A,A) => A): Rand[A] = {

    def balanced(rvs: IndexedSeq[Rand[A]]): Rand[A] =
      if (rvs.size == 1)
        rvs(0)
      else {
        val (lrvs, rrvs) = rvs.splitAt(rvs.size/2)
        balanced(lrvs).map2(balanced(rrvs))(binOp)
      }

    balanced(rands)
  }

  /** Change an IndexedSeq of Rands into a Rand of an IndexedSeq.  */
  def sequenceIndexedSeq[A](rands: IndexedSeq[Rand[A]]): Rand[IndexedSeq[A]] =
    if (rands.isEmpty)
      unit(IndexedSeq())
    else {
      val randV = rands.map(_.map(a => IndexedSeq(a)))
      balancedBinComp(randV)(_ ++ _)
    }

  /** Change a List of rands into a rand of a List. */
  def sequence[A](rands: List[Rand[A]]): Rand[List[A]] =
    sequenceIndexedSeq(rands.toVector) map (_.toList)

}
