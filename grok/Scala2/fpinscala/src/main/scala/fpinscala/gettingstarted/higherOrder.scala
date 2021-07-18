package fpinscala.chap02.gettingstarted

/*
   Currying and partial application
*/

object higherOrder {

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a, b)
  def partial2[A,B,C](b: B, f: (A,B) => C): A => C = a => f(a, b)
  def curry[A,B,C](f: (A,B) => C): A => B => C = a => b => f(a, b)
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a, b) => f(a)(b)
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  val ff = (x: Double) => x*x - 1
  val gg = (x: Double) => 2*x + 1
  val hh = (x: Double) => (x - 1)*(x + 2) - 7

  def poly(x: Double, y: Double) = 3*x*x*y - x*y + 4*x*y*y - 7
  def zz(z: Double) = 1 + 0.5*z + 0.25*z*z

  def main(args: Array[String]) = {
    println("poly(%f, %f) = %f".format(3.0, 2.0, poly(3, 2)))
    println("poly(%f, %f) = %f".format(2.0, 3.0, poly(2, 3)))
    val poly1 = partial1(2.0, poly)
    val poly2 = partial2(3.0, poly)
    println("poly1(%f) = %f".format(3.0, poly1(3)))
    println("poly2(%f) = %f".format(2.0, poly2(2)))
    val foo = curry(poly)
    val bar = foo(2.0)
    println("bar(%f) = %f".format(3.0, bar(3.0)))
    val baz = uncurry(foo)
    println("baz(%f, %f) = %f".format(3.0, 2.0, baz(3.0, 2.0)))
    println("baz(%f, %f) = %f".format(2.0, 3.0, baz(2.0, 3.0)))
    val kk = compose(hh, compose(gg, ff))
    val ll = hh.compose(gg.compose(ff))
    println("hh(gg(ff(%f))) = %f".format(3.0, hh(gg(ff(3.0)))))
    println("kk(%f) = %f".format(3.0, kk(3.0)))
    println("ll(%f) = %f".format(3.0, ll(3.0)))
    val barzz = compose(bar, zz)
    println("bar(zz(%f)) = %f".format(3.0, bar(zz(3.0))))
    println("barzz(%f) = %f".format(3.0, barzz(3.0)))
  }
}
