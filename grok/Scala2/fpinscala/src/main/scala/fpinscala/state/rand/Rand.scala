package fpinscala.state.rand

import fpinscala.state.State

/** Rand case class
  *
  * Represents a random variable in the sense of probability
  * theory.  The underlying probability domain space being
  * any class which extends the fpinscala.state.ran.RNG
  * abstract class.
  */
case class Rand[+A](action: State[RNG, A]) {

  def flatMap[B](f: A => Rand[B]): Rand[B] =
    Rand { action flatMap { a => f(a).action } }

  def map[B](f: A => B): Rand[B] = Rand { action map f }

  def map2[B, C](rv: Rand[B])(f: (A, B) => C): Rand[C] =
    Rand { action.map2(rv.action)(f) }

  /** Produce a definite value by providing the random variable
    * with a value from the underlying probability space.
    *
    * Random variable as in probability theory, not in
    * the "Fortran" sense of a function which produces
    * a different "random" value whenever called.
    */
  def apply(s: RNG): A = action.run(s)._1

}

object Rand {

  def unit[A](a: A): Rand[A] = Rand(State(s => (a, s)))

  /** A simple version of sequence - stackoverflow cranky */
  def sequenceSimple[A](rvs: List[Rand[A]]): Rand[List[A]] =
    rvs.reverse.foldLeft(unit[List[A]](Nil)) { (acc, rv) =>
      rv.map2(acc)(_ :: _)
    }

  /** Combine an indexable collection of random variables
    * into a random variable via a binary operator.
    *
    *   Note: For efficiency, collection assumed nonempty.
    */
  def balancedBinComp[A](
      rands: IndexedSeq[Rand[A]]
  )(binOp: (A, A) => A): Rand[A] = {

    def balanced(rvs: IndexedSeq[Rand[A]]): Rand[A] =
      if (rvs.size == 1)
        rvs(0)
      else {
        val (lrvs, rrvs) = rvs.splitAt(rvs.size / 2)
        balanced(lrvs).map2(balanced(rrvs))(binOp)
      }

    balanced(rands)
  }

  /** Change an IndexedSeq of Rands into a Rand of an IndexedSeq. */
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

  /** State action to generate a random Int.
    *
    * State((state: RNG) => (value: Int, nextState: RNG))
    *
    * Aside: int(rng: RNG) is a random variable, in the sense
    *   of probability theory.  It is a function which maps
    *   values from some probability space (the possible
    *   values of some subclass of RNG) to the space of
    *   32-bit signed integer values (Int).
    *
    *   For the LCG subclass, this mapping is uniform, i.e.
    *   it is equally likely to get any possible Int value.
    */
  def int: Rand[Int] = Rand[Int](State(_.nextInt))

  /** Useful when constructing State actions which generate RNGs */
  def rng: Rand[RNG] = Rand[RNG](State(_.nextRNG))

  /** Generate a random boolean */
  def boolean: Rand[Boolean] = int map { ii => ii % 2 == 0 }

  /** Random action to generate a list from a random action */
  def randList[A](count: Int, ra: Rand[A]): Rand[List[A]] =
    sequence(List.fill(count)(ra))

  /** Random action to generate a list of Int */
  def ints(count: Int): Rand[List[Int]] = randList(count, int)

  def nonNegIntsLessThan(count: Int, bound: Int): Rand[List[Int]] =
    randList(count, nonNegIntLessThan(bound))

  /** Generate a random integer between 0 and Int.maxValue (inclusive) */
  def nonNegInt: Rand[Int] = Rand {
    State {
      _.nextInt match {
        case (ran, rng2) if ran >= 0            => (ran, rng2)
        case (ran, rng2) if ran == Int.MinValue => (0, rng2)
        case (ran, rng2)                        => (-ran, rng2)
      }
    }
  }

  /** Generate an even random integer between 0 and Int.maxValue (inclusive) */
  def nonNegEvenInt: Rand[Int] =
    nonNegInt map { ii => ii - ii % 2 }

  /** Random action non-negative Int less than n
    *
    *   1. Keeps things uniformly distributed over
    *      the range of the random variable even
    *      in the case of n not evenly dividing
    *      the integer value Int.MaxValue + 1.
    *   2. The quantity ii + ((n-1) - mod) is the
    *      the top of the "module band."  If it is
    *      "negative" due to rollover, we redo the
    *      calculation to keep the distribution
    *      uniform.
    *   3. Algorithm assumes that n > 0.
    *   3. Stack overflow can happen if n < 0.
    *   5. For n = 0, you will get a divide by 0
    *      runtime java.lang.ArithmeticException.
    *   6. For efficiency, client responsible to
    *      ensure that n > 0.
    *
    *   Caller be warned, don't call with n <= 0.
    */
  def nonNegIntLessThan(n: Int): Rand[Int] =
    nonNegInt flatMap { ii =>
      val mod = ii % n
      if (ii + ((n - 1) - mod) >= 0)
        unit(mod)
      else
        nonNegIntLessThan(n)
    }

  /** Random Int within the range start <= random_variable < end
    *
    *   Pathological cases:
    *     If start = end, always generate start.
    *     If statt > end, generate start >= random_variable > end
    *
    *   Logic is complicated by the JVM not having unsigned types!
    */
  def exclusiveIntRange(start: Int, end: Int): Rand[Int] =
    if (start > end) {
      exclusiveIntRange(end, start) map { _ + 1 }
    } else {
      if (end - start >= 0) {
        // Normal logic
        val len = if (start != end) end - start else 1
        nonNegIntLessThan(len) map { start + _ }
      } else {
        // integer difference > Int.MaxValue
        val neg = -start
        val nonNeg = end
        val distNonNeg = exclusiveIntRange(0, end)
        if (start > Int.MinValue) {
          val probNeg = neg.toDouble / (neg.toDouble + nonNeg.toDouble)
          val distNeg = exclusiveIntRange(start, 0)
          joint2(probNeg)(distNeg, distNonNeg)
        } else {
          // integer difference for negatives > Int.MaxValue
          val distNegLow = exclusiveIntRange(Int.MinValue, Int.MinValue / 2)
          val distNegHigh = exclusiveIntRange(Int.MinValue / 2, 0)
          joint3(0.25, 0.25)(distNegLow, distNegHigh, distNonNeg)
        }
      }
    }

  /** Generate a random Double between 0 (inclusive) and 1 (exclusive) */
  def double: Rand[Double] = {
    val d = Int.MaxValue.toDouble + 1.0
    nonNegInt map { _.toDouble / d }
  }

  /** Weighted joint distribution of 2 distributon
    *   dist1 with probability prob1
    *   dist2 with probability 1 - prob2
    */
  def joint2[A](
    prob1: Double
  )(
    dist1: Rand[A],
    dist2: Rand[A]
  ): Rand[A] = double flatMap {
      (prob: Double) =>
        if (prob < prob1)
          dist1
        else
          dist2
  }

  /** Weighted joint distribution of 3 distributon
    *   dist1 with probability prob1
    *   dist2 with probability prob2
    *   dist3 with probability 1 - prob1 - prob2
    */
  def joint3[A](
    prob1: Double,
    prob2: Double
  )(
    dist1: Rand[A],
    dist2: Rand[A],
    dist3: Rand[A]
  ): Rand[A] = double flatMap {
      (prob: Double) =>
        if (prob < prob1)
          dist1
        else if (prob < prob1 + prob2)
          dist2
        else
          dist3
  }

  /** Weighted joint distribution of 4 distributon
    *   dist1 with probability prob1
    *   dist2 with probability prob2
    *   dist3 with probability prob3
    *   dist4 with probability 1 - prob1 - prob2 - prob3
    */
  def joint4[A](
    prob1: Double,
    prob2: Double,
    prob3: Double
  )(
    dist1: Rand[A],
    dist2: Rand[A],
    dist3: Rand[A],
    dist4: Rand[A]
  ): Rand[A] = double flatMap {
      (prob: Double) =>
        if (prob < prob1)
          dist1
        else if (prob < prob1 + prob2)
          dist2
        else if (prob < prob1 + prob2 + prob3)
          dist3
        else
          dist4
  }

  /** Weighted joint distribution of 5 distributon
    *   dist1 with probability prob1
    *   dist2 with probability prob2
    *   dist3 with probability prob3
    *   dist3 with probability prob4
    *   dist4 with probability 1 - prob1 - prob2 - prob3 - prob4
    */
  def joint5[A](
    prob1: Double,
    prob2: Double,
    prob3: Double,
    prob4: Double
  )(
    dist1: Rand[A],
    dist2: Rand[A],
    dist3: Rand[A],
    dist4: Rand[A],
    dist5: Rand[A]
  ): Rand[A] = double flatMap {
      (prob: Double) =>
        if (prob < prob1)
          dist1
        else if (prob < prob1 + prob2)
          dist2
        else if (prob < prob1 + prob2 + prob3)
          dist3
        else if (prob < prob1 + prob2 + prob3 + prob4)
          dist4
        else
          dist5
  }

}
