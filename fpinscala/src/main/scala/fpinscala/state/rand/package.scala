package fpinscala.state

/** Package object for fpinscala.state.rand
 *
 *  Used to define the Rand type alias, as
 *  well as associate the val Rand with the
 *  correct companion object, package wide.
 *
 *  Note: There can only be one package object
 *        defined per package.  Usual convention
 *        is to give it the name package.scala.
 *
 *  Note: Modeled after what is done in the
 *        package.scala file for the scala package.
 */
package object rand {
  type Rand[A] = fpinscala.state.State[RNG,A]
  val Rand = fpinscala.state.State
}
