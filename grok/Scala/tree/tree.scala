package tree
// Package ...
//
//   ...

// Define a set of case classes for representing binary trees.
sealed abstract class Tree
case class Node(elem: Int, left: Tree, right: Tree) extends Tree
case object Leaf extends Tree

// Return the in-order traversal sequence of a given tree
def inOrder(t: Tree): List[Int] = t match {
  case Node(e, l, r) => inOrder(l) ::: List(e) ::: inOrder(r)
  case Leaf          => List()
}

@main def start() =
  val foo = 42
  val bar = 13
  val baz = 7
  val myTree = Node(Node(Leaf, foo, Node(Leaf, bar, Leaf), Leaf), baz, Leaf)
  foreach ii in inOrder(myTree)
    println(ii)
