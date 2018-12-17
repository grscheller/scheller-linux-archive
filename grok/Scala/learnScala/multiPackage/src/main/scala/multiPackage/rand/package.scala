package grokScala.grok

/** Package object for grokScala.packageWide
 *
 *  Used to define the Foo type alias, as
 *  well as associate the val Foo with the
 *  correct companion object, package wide.
 *
 *  Note: There can only be one package object
 *        defined per package.  Usual convention
 *        is to give it the name package.scala.
 *
 *  Note: Modeled after what is done in the
 *        package.scala file for the scala package.
 *
 *  Note: Note that the package object itself is
 *        put in the parent package.
 */
package object rand {
  type Rand[A] = grokScala.grok.state.State[RNG,A]
  val Rand = grokScala.grok.state.State
}
