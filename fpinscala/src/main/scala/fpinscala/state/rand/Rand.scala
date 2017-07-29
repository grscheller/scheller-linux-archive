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

  def sequence[A](rvs: List[Rand[A]]): Rand[List[A]] =
    rvs.reverse.foldLeft(unit[List[A]](Nil)) {
      (acc, rv) => rv.map2(acc)(_ :: _)
    }

}
