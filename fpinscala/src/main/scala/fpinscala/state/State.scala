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
 *  the run "method".
 *
 *  note: Actually, as far as the Scala language
 *        is concerned, run is a class member/property
 *        whose value happens to be of type S => (A,S).
 *        Members and methods share the same namespace,
 *        hence the distinction is just an
 *        implementation detail.
 *
 *  The getState and setState combinators
 *  are used to manipulate the state.
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
    flatMap {a => sb map {b => f(a, b)}}

  def both[B](rb: State[S,B]): State[S,(A,B)] =
    map2(rb) { (_, _) }

  // These next two method are what makes
  // this the State Monad.

  /** Return the current state as the value. */
  def getState: State[S,S] = State(s => (s, s))

  /** Manually set a state.
   *
   *    Ignore previous state,
   *    return a meaningless value.
   */
  def setState(s: S): State[S,Unit] = State(_ => ((), s))

  /** Modify the state with a function,
   *
   *    Included here more to illustrate
   *    the use case of get and set.
   */
  def modifyState(f: S => S): State[S, Unit] =
    for {
      s <- getState
      _ <- setState(f(s))
    } yield ()

  /** Run the action, extract the value, ignore next state.
   *
   *    A convenience method to extract a final result
   *    to avoid having to pattern match the value out.
   *    Has the virtue of hiding the tuple used in
   *    the implemention of the State monad.
   *
   *    Use case:
   *      val a = action(s)
   *      val a = action apply s
   *
   *    same as
   *      val a = action.run(s)._1
   *      val a = (action run s)._1
   *      val (a, _) = action.run(s)
   *      val (a, _) = action run s
   *
   */
  def apply(s: S): A = run(s)._1

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
   *    makes sense since I only need an (a: A),
   *    and not a State[S,A], to use it.
   */
  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.reverse.foldLeft(unit[S,List[A]](Nil)) {
      (acc, f) => f.map2(acc)(_ :: _)
    }

}
