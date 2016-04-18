package grockScala.datastructures

/** Implement a Tree ADT where the data lives in the Leaves */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  
}
