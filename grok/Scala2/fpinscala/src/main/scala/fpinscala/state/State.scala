package fpinscala.state

/** State Case Class:
  *
  *  For dealing with stateful programs in
  *  a purely functional way.
  *
  *  Abstracting what I did for the RNG
  *  pseudo-random number generator trait
  *  in package fpinscala.rngStandalone.
  *
  *  Use case for RNG whould be to use
  *
  *    type Rand[A] = State[RNG, A]
  *
  *  or better yet
  *
  *    case class Rand[+A](action: State[RNG,A])
  *
  *  instead of
  *
  *    type Rand[A] = RNG => (A, RNG)
  *
  *  The get and set combinators in the companion object
  *  are used to manipulate the state.  They are what
  *  makes this the "State" monad.
  */
case class State[S, +A](run: S => (A, S)) {

  import State.unit

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      g(a) run s1
    }

  def map[B](f: A => B): State[S, B] =
    flatMap { a => unit(f(a)) }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb map { b => f(a, b) }
    }

  def both[B](rb: State[S, B]): State[S, (A, B)] =
    map2(rb) { (_, _) }

}

/** Utility functions for State case class. */
object State {

  /** Create a State action from a value.
    *
    *    Placed in companion object otherwise compiler
    *    tells me I am using a covariant variable in a
    *    contravariant position.
    *
    *    Putting in the companion objects actually
    *    makes sense since I only need an (a: A),
    *    and not a State[S,A], to use it.
    */
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  /** Get the state.
    *
    *  In the run action, return the
    *  current state as the value.
    */
  def get[S]: State[S, S] = State(s => (s, s))

  /** Manually set a state.
    *
    *  The run action ignores previous state,
    *  returns the canonical meaningless value.
    */
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  /** Modify the state with a function.
    *
    *  Illustrates the use case of get and set.
    */
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  /** Original sequence implementation - stackoverflow cranky */
  def sequenceSimple[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](Nil)) { (acc, f) =>
      f.map2(acc)(_ :: _)
    }

  /** Combine an indexable collection of state actions into
    *  a single state action via a binary operator.
    *
    *    Note: For efficiency, collection assumed nonempty.
    */
  def balancedBinComp[S, A](stateActions: IndexedSeq[State[S, A]])(binOp: (A, A) => A): State[S, A] = {

    def balanced(sas: IndexedSeq[State[S, A]]): State[S, A] =
      if (sas.size == 1)
        sas(0)
      else {
        val (lsas, rsas) = sas.splitAt(sas.size / 2)
        balanced(lsas).map2(balanced(rsas))(binOp)
      }

    balanced(stateActions)
  }

  /** Change an IndexedSeq of state actions into a state action of an IndexedSeq  */
  def sequenceIndexedSeq[S, A](stateActions: IndexedSeq[State[S, A]]): State[S, IndexedSeq[A]] =
    if (stateActions.isEmpty)
      unit(IndexedSeq())
    else {
      val randV = stateActions.map(_.map(a => IndexedSeq(a)))
      balancedBinComp(randV)(_ ++ _)
    }

  /** Change a List of state actions into a state action of a List. */
  def sequence[S, A](stateActions: List[State[S, A]]): State[S, List[A]] =
    sequenceIndexedSeq(stateActions.toVector) map (_.toList)

}
