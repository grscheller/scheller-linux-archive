package fpinscala.chap03.datastructures

import fpinscala.datastructures._

object ListTest {

  /** Test package */
  def main(args: Array[String]) = {

    // Some data to test with
    val foo1234 = List(1,2,3,4)
    val doo12345 = List(1,2,3,4,5): List[Double]
    val doo12045 = List(1,2,0,4,5): List[Double]
    val bar12345 = List("one", "two", "three", "four", "five")
    val bar12045 = List("one", "two", "", "four", "five")

    // Test tailTot
    print("List.tailT(")
    print(foo1234)
    print(") is ")
    println(List.tailT(foo1234))

    print("List.tailT(")
    print(bar12345)
    print(") is ")
    println(List.tailT(bar12345))

    print("List.tailT(")
    print(Nil)
    print(") is ")
    println(List.tailT(Nil))
    println()

    // Test setHead
    print("List.setHead")
    print(foo1234, 5)
    print(" is ")
    println(List.setHead(foo1234, 5))

    print("List.setHead")
    print(bar12345, "six")
    print(" is ")
    println(List.setHead(bar12345, "six"))

    print("List.setHead")
    print(Nil, 2.0: Double)
    print(" is ")
    println(List.setHead(Nil, 2.0: Double))
    println()

    //Test drop
    print("List.drop")
    print(bar12345, 3)
    print(" is ")
    println(List.drop(bar12345, 3))

    print("List.drop")
    print(bar12345, 0)
    print(" is ")
    println(List.drop(bar12345, 0))

    print("List.drop")
    print(bar12345, 12)
    print(" is ")
    println(List.drop(bar12345, 12))
    println()

    // Test dropWhile
    print("List.dropWhile")
    print(foo1234, " (3 > _)")
    print(" is ")
    println(List.dropWhile(foo1234, (3 > (_: Int))))

    print("List.dropWhile")
    print(foo1234, " (x: Int) => x % 2 != 0")
    print(" is ")
    println(List.dropWhile(foo1234, (x: Int) => x % 2 != 0))

    print("List.dropWhile")
    print(bar12345, " (x: String) => x.contains(\"o\")")
    print(" is ")
    println(List.dropWhile(bar12345, (x: String) => x.contains("o")))

    print("List.dropWhile1")
    print(bar12345, " (x: String) => x.contains(\"o\")")
    print(" is ")
    println(List.dropWhile1(bar12345, (x: String) => x.contains("o")))

    print("List.dropWhile2(")
    print(bar12345)
    print(")(x => x.contains(\"o\"))")
    print(" is ")
    println(List.dropWhile2(bar12345)(x => x.contains("o")))
    println()

    // Test init - exercise 3.6
    print("List.init(")
    print(bar12345)
    print(") is ")
    println(List.init(bar12345))
    println()

    // Test productL1 and productR1
    print("List.productL1(")
    print(doo12345)
    print(") is ")
    println(List.productL1(doo12345))

    print("List.productR1(")
    print(doo12045)
    print(") is ")
    println(List.productR1(doo12045))
    println()

    // Exercise 3.7
    // Test productSC
    print("List.productSC(")
    print(doo12345)
    print(") is ")
    println(List.productSC(doo12345))

    print("List.productSC(")
    print(doo12045)
    print(") is ")
    println(List.productSC(doo12045))
    println()

    // Test catSC
    print("List.catSC(")
    print(bar12345)
    print(") is ")
    println(List.catSC(bar12345))

    print("List.catSC(")
    print(bar12045)
    print(") is ")
    println(List.catSC(bar12045))
    println()

    // Find length of List (Exercise 3.9)
    print("List.length(List()) is ")
    println(List.length(List()))

    print("List.length(List(0 to 10000)) is ")  // A list of one iterator!
    println(List.length(List(0 to 10000)))

    var as = List(): List[Int]
    for (a <- 0 to 15) as = Cons(a, as)
    print("List.length(")
    print(as)
    print(") = ")
    println(List.length(as))
    print("List.lengthL(")
    print(as)
    print(") = ")
    println(List.lengthL(as))
    println()

    // Test foldLeft
    as = List()
    for (a <- 1 to 1000) as = Cons(a, as)
    print("Sum of 1 to 100 with foldLeft is ")
    println(List.foldLeft(as, 0)(_ + _))
    println()

    // Test foldLeftSC
    var cs = List(1, 2, 3, 4, 5, 0, 6, 7)
    print("List.foldLeftSC(")
    print(cs)
    print(", 0, 1)(_ * _) = ")
    println(List.foldLeftSC(cs, 0, 1)(_ * _))

    cs = List(1, 2, 3, 4, 5)
    print("List.foldLeftSC(")
    print(doo12345)
    print(", 0.0, 1.0)(_ * _) = ")
    println(List.foldLeftSC(doo12345, 0.0, 1.0)(_ * _))
    println()

    // Test foldRightUnsafe
    print("Sum of 1 to 100 with List.foldRightUnsafe is ")
    println(List.foldRightUnsafe(as, 0)(_ + _))
    println()

    // Test foldRight
    var bs = List(): List[Long]
      println("Start for loop")
    for (b <- 1L to 200000L) bs = Cons(b, bs) // Takes a while ???
      println("Finish for loop")
    print("Sum of 1 to 200000 with foldLeft is ")
    println(List.foldLeft(bs, 0L)(_ + _))
    print("Sum of 1 to 200000 with foldRight is ")
    println(List.foldRight(bs, 0L)(_ + _))
    println()

    // Test reverse
    print("List.drop(List.reverse(bs), 199995)) = "); println(List.drop(List.reverse(bs), 199995))
    println()

    // Test append
    print("List.append")
    print((doo12345, doo12045))
    print(" = ")
    println(List.append(doo12345, doo12045))
    println()

    // Test flatten
    val ll = List(List(4,5,6), List(3,2), List(), List(10,11,12,13))
    print("List.flatten(")
    print(ll)
    print(") = ")
    println(List.flatten(ll))

    val emptyList = List()
    print("List.flatten(")
    print(emptyList)
    print(") = ")
    println(List.flatten(emptyList))
    println()

    // Test bump1
    print("List.bump1(")
    print(foo1234)
    print(") = ")
    println(List.bump1(foo1234))
    println()

    // Test doublesToStrings
    print("List.doublesToStrings(")
    print(doo12345)
    print(") = ")
    val dooStrings = List.doublesToStrings(doo12345)
    println(dooStrings)

    val thirdElement = List.head(List.drop(dooStrings,2))
    print("List.head(List.drop(dooStrings,2))) = ")
    println(List.head(List.drop(dooStrings,2)))
    print("List.head(List.drop(dooStrings,2))).reverse = ")
    println(List.head(List.drop(dooStrings,2)).reverse)
    println()

    // Test map
    print("List.map(")
    print(foo1234)
    print(")((x: Int) => math.exp(x) - 5.0) = ")
    println(List.map(foo1234)((x: Int) => math.exp(x) - 5.0))
    println()

    // Test filter: filter out odd values
    print("List.filter(")
    print(foo1234)
    print(")(_ % 2 != 1) = ")
    println(List.filter(foo1234)(_ % 2 != 1))
    println()

    // Test flatmap
    print("List.flatMap(")
    print(foo1234)
    print(")(i => List(i, i*i)) = ")
    println(List.flatMap(foo1234)(i => List(i, i*i)))
    println()

    // Test filter2: filter out even values
    print("List.filter2(")
    print(foo1234)
    print(")(_ % 2 == 1) = ")
    println(List.filter2(foo1234)(_ % 2 == 1))
    println()

    // Compare filter and filter2
    var aa = List(): List[Int]
    var bb = List(): List[Int]
    var cc = List(): List[Int]
    for (a <- 1 to 70000) aa = Cons(a, aa)
    for (b <- 60000 to 65000) bb = Cons(b, bb)
    for (c <- 41000 to 40000 by -1) cc = Cons(c, cc)
    val a1 = List.drop(aa, 10000)
    val a2 = a1

    print("List.filter(a1)(_ < 20) = ")
    println(List.filter(a1)(_ < 20))

    print("List.filter2(a2)(_ < 15) = ")
    println(List.filter2(a2)(_ < 15))
    println()

    // Test addLists
    val first = List(42, 15, 0, 11, 5, 12, 3)
    val second = List(1, 2, 3, 4, 5)
    print("List.addLists(")
    print(first); print(", "); print(second)
    print(") = \n    ")
    println(List.addLists(first, second))
    println()

    // Test zipWith
    print("List.zipWith(")
    print(first); print(", "); print(second)
    print(")( _ * _ ) = \n    ")
    println(List.zipWith(first, second)(_ * _))
    println()

    // Test hasSubsequence
    val third = List(11, 5, 12)
    val fourth = List(15, 3, 4, 5, 6, 7, 8)
    val fifth = List(): List[Int]

    print(first)
    if (List.hasSubsequence(first, second))
      print(" has ")
    else
      print(" does not have ")
    print(second)
    println(" as a subsequence")

    print(first)
    if (List.hasSubsequence(first, third))
      print(" has ")
    else
      print(" does not have ")
    print(third)
    println(" as a subsequence")

    print(first)
    if (List.hasSubsequence(first, fourth))
      print(" has ")
    else
      print(" does not have ")
    print(fourth)
    println(" as a subsequence")

    print(first)
    if (List.hasSubsequence(first, fifth))
      print(" has ")
    else
      print(" does not have ")
    print(fifth)
    println(" as a subsequence")

    print(fifth)
    if (List.hasSubsequence(fifth, fifth))
      print(" has ")
    else
      print(" does not have ")
    print(fifth)
    println(" as a subsequence")

    print(fifth)
    if (List.hasSubsequence(fifth, first))
      print(" has ")
    else
      print(" does not have ")
    print(first)
    println(" as a subsequence")

    println("Large List hasSubsequence tests:")
    if (List.hasSubsequence(aa, bb))
      println("\tPASS")
    else
      println("\tFAIL")
    if (List.hasSubsequence(aa, cc))
      println("\tFAIL")
    else
      println("\tPASS")
    if (List.hasSubsequence(bb, bb))
      println("\tPASS")
    else
      println("\tFAIL")

  }
}
