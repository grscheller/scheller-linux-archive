package comp.adt

// Example of product types

/* This one essentially the same as a Tuple3.
 * It has 8 values if A = B = C = Boolean
 */
case class Eight[A,B,C](a: A, b: B, c: C)

// Examples of sum types

/* This one has 6 distinct values */
sealed trait Six
case class Bool1(n: Boolean) extends Six
case class Bool2(n: Boolean) extends Six
case class Bool3(n: Boolean) extends Six

sealed trait IntOrString
case class AnInt(i: Int) extends IntOrString
case class AnString(s: String) extends IntOrString

// Test the above.
object ADT {

  def main(args: Array[String]) = {
    println("See if it compiles")
  }
}
