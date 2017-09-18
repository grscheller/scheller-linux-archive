/** Parallel Properties package.
 *
 *    Par trait utilities used with the property based
 *    testing package fpinscala.testing.
 *
 */
package fpinscala.parallelism

import java.util.concurrent.{ExecutorService => ES}
import fpinscala.testing.{Prop, Gen}

/** Par companion object */
object ParProp {

  def forAllPar[A](g: Gen[A], esPool: Gen[ES])(f: A => Par[Boolean]): Prop =
    Prop.forAll(g.map2(esPool)((_,_))) {
      case (a,es) => f(a).run(es)
    }

}
