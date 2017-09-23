/** Parallel Properties package.
 *
 *    Par trait utilities used with the property based
 *    testing package fpinscala.testing.
 *
 */
package fpinscala.parallelism

import java.util.concurrent.{ExecutorService => ES}
import fpinscala.testing.{Prop, Gen}
import Gen.**

/** ParProp singleton object
 *
 *  Object containing a method for use with the testing package
 *  fpiscala.testing for testing functionality specific to the
 *  fpinscala.parallelism package.
 *  
 *  @note Implemented as a name space for an additional Prop "method."
 *  @note Saves the overhead of full-blown inheriting from Prop.
 *
 */
object ParProp {

  def forAllPar[A](g: Gen[A], esGen: Gen[ES])(f: A => Par[Boolean]): Prop =
    Prop.forAll(g ** esGen) {
      case a ** es  => f(a).run(es)
    }

}
