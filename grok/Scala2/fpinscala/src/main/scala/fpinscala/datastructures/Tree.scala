package fpinscala.datastructures

/** Implement a Tree ADT where the data lives in the Leaves */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  /** Return size of a Tree: leaves + branches */
  def size1[A](tree: Tree[A]): Int =
    tree match {
      case Branch(left,right) => size1(left) + size1(right) + 1
      case _ => 1
    }

  /** Return number of leaves in a Tree */
  def numLeaves1[A](tree: Tree[A]): Int =
    tree match {
      case Branch(left,right) => numLeaves1(left) + numLeaves1(right)
      case _ => 1
    }

  // Exercise 3.26
  /** Returns the maximum element in a Tree[Int] */
  def maximum1(tree: Tree[Int]): Int =
    tree match {
      case Branch(left,right) => maximum1(left) max maximum1(right)
      case Leaf(ii) => ii
    }

  // Exercise 3.27
  /** Returns the maximum path length from the root
   *  to any Leaf in the tree
   */
  def depth1[A](tree: Tree[A]): Int =
    tree match {
      case Branch(left,right) => (depth1(left) max depth1(right)) + 1
      case _ => 1
    }

  /** Map a function over the Leaves of a Tree */
  def map1[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Branch(left,right) => Branch(map1(left)(f), map1(right)(f))
      case Leaf(a) => Leaf(f(a))
    }

  // Exercise 3.28 - Generalize above functions and reimplement
  //** A "fold" function for the Tree type */
  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B =
    tree match {
      case Branch(left,right) => g(fold(left)(f)(g), fold(right)(f)(g))
      case Leaf(a) => f(a)
    }

  /** Return size of a Tree: leaves + branches */
  def size[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ + _ + 1)

  /** Return number of leaves in a Tree */
  def numLeaves[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ + _)

  /** Returns the maximum element in a Tree[Int] */
  def maximum(tree: Tree[Int]): Int =
    fold(tree)(ii => ii)(_ max _)

  /** Returns the maximum path length from the root
   *  to any Leaf in the tree
   */
  def depth[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((ii,jj) => (ii max jj) + 1)

  /** Map a function over the Leaves of a Tree */
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))  // Needs type hint

}
