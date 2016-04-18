package grockScala.datastructures

/** Implement a Tree ADT where the data lives in the Leaves */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

//  def apply[A](as: A*): Tree[A] =
//    if (as.isEmpty) Nil
//    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.25
  /** Return size of a Tree: leaves + branches 
   *
   *  Not tail recursive - may become a problem if tree
   *  is not well balanced
   */
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Branch(left, right) => size(left) + size(right) + 1
      case _ => 1
    }

  /** Return number of leaves in a Tree */
  def numLeaves[A](tree: Tree[A]): Int =
    tree match {
      case Branch(left, right) => numLeaves(left) + numLeaves(right)
      case _ => 1
    }

  // Exercise 3.26
  /** Returns the maximum element in a Tree[Int] */
  def maximum(tree: Tree[Int]): Int = 
    tree match {
      case Branch(left, right) => maximum(left) max maximum(right)
      case Leaf(ii) => ii
    }

  // Exercise 3.27
  /** Returns the maximum path length from the root
   *  to any Leaf in the tree
   */
  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Branch(left, right) => (depth(left) max depth(right)) + 1
      case _ => 1
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(a) => Leaf(f(a))
    }

}
