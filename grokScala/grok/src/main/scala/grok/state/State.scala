package grokScala.grok.state

/** State Case Class:
 *
 *  Repeating what I did in the fpinscala project.
 *
 */
case class State[S,+A](run: S => (A,S)) {

  import State.unit

  def flatMap[B](g: A => State[S,B]): State[S,B] =
    State { s => 
        val (a, s1) = run(s)
        g(a) run s1
    }

  def map[B](f: A => B): State[S,B] =
    flatMap {a => unit(f(a))}

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap {a => sb map {b => f(a, b)}}

  /** Run the action, extract the value, ignore next state.
   *
   *    A convenience method to extract a final result
   *    to avoid having to pattern match the value out.
   *
   */
  def apply(s: S): A = run(s)._1

}

/** Utility functions for State case class.  */
object State {

  /** Create a State action from a value.  */
  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  /** Turn a List of state actions into a state action of a list */
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.reverse.foldLeft(unit[S,List[A]](Nil)) {
      (acc, f) => f.map2(acc)(_ :: _)
    }

  /** Get the state.
   *
   *  In the run action, return the
   *  current state as the value.
   */
  def get[S]: State[S,S] = State(s => (s, s))

  /** Manually set a state.
   *
   *  The run action ignores previous state,
   *  returns the canonical meaningless value.
   */
  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

  /** Modify the state with a function.
   *
   *  Illustrates the use case of get and set.
   */
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

}
