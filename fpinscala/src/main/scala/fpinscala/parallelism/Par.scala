package fpinscala.parallelism

/** Par Case Class:
 *
 */
case class Par[A](a: A) {
  // Nobody home???
  //
  // Signatures the book gave indicated that all
  // the methods go into the companion object.
  // Without that hint, I probably would have put
  // map2 and run here.
  //
  // A quick peek at the book exercise template
  // showed only a companion object.  In retrospect,
  // having a Par case class makes no sense. What
  // exactly sould pattern matching produce?  Par does
  // not contain a value; it contains something far
  // less substantial.
  // 
}

/** Utility functions for State case class.  */
object Par {

  def unit[A](a: A): Par[A] = Par(a)

//  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C]

//  def fork[A](pa: => Par[A]): Par[A]

//  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

//  def run[A](pa: Par[A]): A
}
