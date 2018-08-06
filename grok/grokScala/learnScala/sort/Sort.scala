package grokScala.sort

object Sort {

  /** Sort a polymorphic ordered list. */
  def sort[A: Ordering](as: List[A]): List[A] =
    as.foldRight(Nil: List[A])(insert[A])

  /** Insert an element into a sorted list. */
  def insert[A: Ordering](x: A, xs: List[A]): List[A] =
    xs match {
      case Nil => x :: Nil
      case y :: ys =>
        if (Ordering[A].lteq(x, y))   // `if (x <= y)' does not work???
          x :: xs
        else
          y :: insert(x, ys)
    }


  def main(args: Array[String]) = {
    val piDigits: List[Int] = List(3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3)

    println(s"Digits of PI: ${piDigits}")
    println(s"Sorted digits of PI: ${sort(piDigits: List[Int])}")

  }

}
