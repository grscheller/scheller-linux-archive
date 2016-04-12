import grockScala.datastructures._

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

    // Test init - exercise 3.6
    print("List.init(")
    print(bar12345)
    print(") is ")
    println(List.init(bar12345))

    // Test productL1 and productR1
    print("List.productL1(")
    print(doo12345)
    print(") is ")
    println(List.productL1(doo12345))

    print("List.productR1(")
    print(doo12045)
    print(") is ")
    println(List.productR1(doo12045))

    print("List.productSC(")
    print(doo12345)
    print(") is ")
    println(List.productSC(doo12345))

    print("List.productSC(")
    print(doo12045)
    print(") is ")
    println(List.productSC(doo12045))

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

    // Test catSC
    print("List.catSC(")
    print(bar12345)
    print(") is ")
    println(List.catSC(bar12345))

    print("List.catSC(")
    print(bar12045)
    print(") is ")
    println(List.catSC(bar12045))

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

    // Test foldLeft
    as = List()
    for (a <- 1 to 1000) as = Cons(a, as)
    print("Sum of 1 to 100 with foldLeft is ")
    println(List.foldLeft(as, 0)(_ + _))

    // Test foldRightUnsafe
    print("Sum of 1 to 100 with List.foldRightUnsafe is ")
    println(List.foldRightUnsafe(as, 0)(_ + _))

    // Test foldRight
    var bs = List(): List[Long]
      println("Start for loop")
    for (b <- 1L to 200000L) bs = Cons(b, bs) // Takes a while ???
      println("Finish for loop")
    print("Sum of 1 to 200000 with foldLeft is ")
    println(List.foldLeft(bs, 0L)(_ + _))
    print("Sum of 1 to 200000 with foldRight is ")
    println(List.foldRight(bs, 0L)(_ + _))

    // Test reverse
    print("List.drop(List.reverse(bs), 199995)) = "); println(List.drop(List.reverse(bs), 199995))

    // Test append
    print("List.append")
    print((doo12345, doo12045))
    print(" = ")
    println(List.append(doo12345, doo12045))
    
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

    // Test bump1
    print("List.bump1(")
    print(foo1234)
    print(") = ")
    println(List.bump1(foo1234))

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

    // Test map
    print("List.map(")
    print(foo1234)
    print(")((x: Int) => math.exp(x) - 5.0) = ")
    println(List.map(foo1234)((x: Int) => math.exp(x) - 5.0))
    
    // Test filter: filter out odd values
    print("List.filter(")
    print(foo1234)
    print(")(_ % 2 != 1) = ")
    println(List.filter(foo1234)(_ % 2 != 1))

    // Test flatmap
    print("List.flatMap(")
    print(foo1234)
    print(")(i => List(i, i*i)) = ")
    println(List.flatMap(foo1234)(i => List(i, i*i)))

    // Test filter2: filter out even values
    print("List.filter2(")
    print(foo1234)
    print(")(_ % 2 == 1) = ")
    println(List.filter2(foo1234)(_ % 2 == 1))

    // Compare filter and filter2
    var aa = List(): List[Int]
    for (a <- 1 to 700000) aa = Cons(a,aa)
    val a1 = List.drop(aa, 10000)
    val a2 = a1
    
    print("List.filter(a1)(_ < 20) = ")
    println(List.filter(a1)(_ < 20))

    print("List.filter2(a2)(_ < 15) = ")
    println(List.filter2(a2)(_ < 15))

  }
}
