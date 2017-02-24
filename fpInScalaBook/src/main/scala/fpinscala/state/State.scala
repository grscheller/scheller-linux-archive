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
 *    type Rand[A] = State[RNG, A]
 *  instead of 
 *    type Rand[A] = RNG => (A, RNG) 
 *  where the lambda is embedded in the
 *  the state case class via the 
 *  State constuctor and is accessed via
 *  the run method.
 *
 *  At this point, I am not sure what
 *  is being abstracted with the get
 *  and set methods.
 *  
 */
case class State[S,+A](run: S => (A,S)) {
  
}

/** Object to provide namespace for RNG trait utility functions.  */
object State {

  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}
