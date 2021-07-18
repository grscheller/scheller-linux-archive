package fpinscala.chap03.datastructures

import fpinscala.datastructures._

object TreeTest {

  /** Test package */
  def main(args: Array[String]) = {

    // Exercise 3.25 - Test size1 and size)

    // Some data to test with
    val leaf5 = Leaf(5.0): Tree[Double]
    val leaf42 = Leaf(42.0): Tree[Double]
    val leafPI = Leaf(Math.PI)  // Type inference works
    val tree3 = Branch(leaf5, leaf42)
    val tree5 = Branch(tree3, leafPI)
    val tree9 = Branch(tree3, tree5)
    val tree11 = Branch(tree9, leaf42)
    val tree21 = Branch(tree11, tree9)

    println("Test Tree.size1:")
    print("Tree.size1(leaf5) = "); println(Tree.size1(leaf5))
    print("Tree.size1(tree3) = "); println(Tree.size1(tree3))
    print("Tree.size1(tree5) = "); println(Tree.size1(tree5))
    print("Tree.size1(tree9) = "); println(Tree.size1(tree9))
    print("Tree.size1(tree11) = "); println(Tree.size1(tree11))
    print("Tree.size1(tree21) = "); println(Tree.size1(tree21))

    println("\nTest Tree.size:")
    print("Tree.size(leaf5) = "); println(Tree.size(leaf5))
    print("Tree.size(tree3) = "); println(Tree.size(tree3))
    print("Tree.size(tree5) = "); println(Tree.size(tree5))
    print("Tree.size(tree9) = "); println(Tree.size(tree9))
    print("Tree.size(tree11) = "); println(Tree.size(tree11))
    print("Tree.size(tree21) = "); println(Tree.size(tree21))

    //Test numLeaves1 and numLeaves
    println("\nTest Tree.numLeaves1:")
    print("Tree.numLeaves1(leaf5) = "); println(Tree.numLeaves1(leaf5))
    print("Tree.numLeaves1(tree3) = "); println(Tree.numLeaves1(tree3))
    print("Tree.numLeaves1(tree5) = "); println(Tree.numLeaves1(tree5))
    print("Tree.numLeaves1(tree9) = "); println(Tree.numLeaves1(tree9))
    print("Tree.numLeaves1(tree11) = "); println(Tree.numLeaves1(tree11))
    print("Tree.numLeaves1(tree21) = "); println(Tree.numLeaves1(tree21))

    println("\nTest Tree.numLeaves:")
    print("Tree.numLeaves(leaf5) = "); println(Tree.numLeaves(leaf5))
    print("Tree.numLeaves(tree3) = "); println(Tree.numLeaves(tree3))
    print("Tree.numLeaves(tree5) = "); println(Tree.numLeaves(tree5))
    print("Tree.numLeaves(tree9) = "); println(Tree.numLeaves(tree9))
    print("Tree.numLeaves(tree11) = "); println(Tree.numLeaves(tree11))
    print("Tree.numLeaves(tree21) = "); println(Tree.numLeaves(tree21))

    // Exercise 3.26 - Test maximum1 amd maximum

    // Some data to test with
    val ileaf5 = Leaf(5): Tree[Int]
    val ileaf42 = Leaf(42): Tree[Int]
    val ileaf37 = Leaf(37): Tree[Int]
    val itree3 = Branch(ileaf5, ileaf37)
    val itree5 = Branch(itree3, ileaf37)
    val itree9 = Branch(itree3, itree5)
    val itree11 = Branch(itree9, ileaf42)
    val itree21 = Branch(itree11, itree9)

    println("\nTest Tree.maximum1:")
    print("Tree.maximum1(ileaf5) = "); println(Tree.maximum1(ileaf5))
    print("Tree.maximum1(itree3) = "); println(Tree.maximum1(itree3))
    print("Tree.maximum1(itree5) = "); println(Tree.maximum1(itree5))
    print("Tree.maximum1(itree9) = "); println(Tree.maximum1(itree9))
    print("Tree.maximum1(itree11) = "); println(Tree.maximum1(itree11))
    print("Tree.maximum1(itree21) = "); println(Tree.maximum1(itree21))

    println("\nTest Tree.maximum:")
    print("Tree.maximum(ileaf5) = "); println(Tree.maximum(ileaf5))
    print("Tree.maximum(itree3) = "); println(Tree.maximum(itree3))
    print("Tree.maximum(itree5) = "); println(Tree.maximum(itree5))
    print("Tree.maximum(itree9) = "); println(Tree.maximum(itree9))
    print("Tree.maximum(itree11) = "); println(Tree.maximum(itree11))
    print("Tree.maximum(itree21) = "); println(Tree.maximum(itree21))

    // See how it displays
    print("\nitree9 = "); println(itree9)

    // Exercise 3.27 - Test depth1 and depth
    println("\nTest Tree.depth1:")
    print("Tree.depth1(leaf5) = "); println(Tree.depth1(leaf5))
    print("Tree.depth1(tree3) = "); println(Tree.depth1(tree3))
    print("Tree.depth1(tree5) = "); println(Tree.depth1(tree5))
    print("Tree.depth1(tree9) = "); println(Tree.depth1(tree9))
    print("Tree.depth1(tree11) = "); println(Tree.depth1(tree11))
    print("Tree.depth1(tree21) = "); println(Tree.depth1(tree21))

    println("\nTest Tree.depth:")
    print("Tree.depth(leaf5) = "); println(Tree.depth(leaf5))
    print("Tree.depth(tree3) = "); println(Tree.depth(tree3))
    print("Tree.depth(tree5) = "); println(Tree.depth(tree5))
    print("Tree.depth(tree9) = "); println(Tree.depth(tree9))
    print("Tree.depth(tree11) = "); println(Tree.depth(tree11))
    print("Tree.depth(tree21) = "); println(Tree.depth(tree21))

    print("\ntree21 = "); println(tree21)

    // Exercise 3.28 - Test map1 and map
    println("\nTest Tree.map1:")
    print("Tree.map1(tree21)((x) => x*x - 1) = ")
    println( Tree.map1(tree21)((x) => x*x - 1) )

    println("\nTest Tree.map:")
    print("Tree.map(tree21)((x) => x*x - 1) = ")
    println( Tree.map(tree21)((x) => x*x - 1) )

  }
}
