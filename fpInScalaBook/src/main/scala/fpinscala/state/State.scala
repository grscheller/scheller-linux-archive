package fpinscala.state

import State._

/** State Case Class:
 *
 *  For dealing with stateful programs in
 *  a purely functional way.
 *
 *  Abstracting what I did for the RNG
 *  pseudo-random number generators.
 *
 *  Use case for RNG whould be to use
 *
 *    type Rand[A] = State[RNG, A]
 *
 *  instead of 
 *
 *    type Rand[A] = RNG => (A, RNG) 
 *
 *  where the lambda/Function1 is embedded
 *  in the state case class via the 
 *  State constuctor and is accessed via
 *  the run "method" [actually, run is a
 *  val whose value happens to be of
 *  type S => (A,S)].
 *
 *  The get and set combinators are used to
 *  manipulate the state.
 *  
 */
case class State[S,+A](run: S => (A,S)) {

  import State.unit

  def flatMap[B](g: A => State[S,B]): State[S,B] =
    State(
      s => {
        val (a, s1) = run(s)
        g(a) run s1
      }
    )

  def map[B](f: A => B): State[S,B] =
    flatMap {a => unit(f(a))}

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap {a => sb flatMap {b => unit(f(a, b))}}

  /** Return the current state as the value. */
  def get: State[S,S] = State(s => (s, s))

  /** Manually set a state.
   *
   *    Ignore previous state,
   *    return a meaningless value.
   */
  def set(s: S): State[S,Unit] = State(_ => ((), s))

  /** Modify the state with a function,
   *
   *    Included here more to illustrate
   *    the use case of get and set.
   */
  def modify(f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

}

/** Utility functions for State case class.  */
object State {

  /** Create a State action from a value.
   *
   *    Placed in companion object otherwise compiler
   *    tells me I am using a covariant variable in a
   *    contravariant position.
   *
   *    Putting in the companion objects actually 
   *    makes sense since I only need an a: A, and
   *    not a State[S,A], to use it.
   */
  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.reverse.foldLeft(unit[S,List[A]](Nil)) {
      (acc, f) => f.map2(acc)(_ :: _)
    }

}
