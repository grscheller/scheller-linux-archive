/** Parallel Properties package.
 *
 *    Java Future utilities used with the property based
 *    testing package fpinscala.testing.
 *
 */
package fpinscala.parallelism.javaFutures

import java.util.concurrent.{ExecutorService => ES}
import fpinscala.testing.{Prop, Gen, SGen}
import Gen.**

/** ParProp singleton object
 *
 *  Object containing methods for use with the testing package
 *  fpiscala.testing for testing functionality specific to the
 *  fpinscala.parallelism package.
 *
 *  @note Implemented as a name space for an additional Prop "method."
 *  @note Saves the overhead of full-blown inheriting from Prop.
 *
 */
object ParProp {

  /** Combine 2 Gen's, the second one being Gen[ES]
   *
   *  @note Best use case is to choose ammoung several
   *        preexisting ES's.  Creating the ES within the
   *        context of the Gen.sample method gives no way to
   *        shut it down later.
   *  @note In implementation, could have just called run method,
   *        but wanted to keep analogous with book version.
   */
  def forAllPar[A](g: Gen[A], esGen: Gen[ES])(f: A => Par[Boolean]): Prop =
    Prop.forAll(g ** esGen) {
      case a ** es => f(a).future(es).get
    }

  /** SGen version.
   *
   *  @note Second argument still a Gen[ES].
   */
  def forAllPar[A](sg: SGen[A], esGen: Gen[ES])(f: A => Par[Boolean]): Prop =
    Prop.forAll(sg ** esGen.unsized) {
      case a ** es => f(a).future(es).get
    }

}
