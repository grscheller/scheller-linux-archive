package grockScala.test.laziness

import grockScala.laziness._

object StreamTest{

  // A really poor implementation to calculating fibonacci numbers.
  // To model an expensive calculation.
  def fibPoor(n: Int): Int =
    if (n < 2) n else fibPoor(n-1) + fibPoor(n-2)

  def main(args: Array[String]): Unit = {

    // Test constructors

    val foo = Stream(2.1, 4.2, 5.3, 1.4, 8.5)
    val baz = Stream(42)
    val bar: Stream[Int] = Stream()

    println("\nDatastructures used:\n")
    println("foo = Stream(2.1, 4.2, 5.3, 1.4, 8.5)")
    println("baz = Stream(42)")
    println("bar = Stream()")

    // Test headOption and tailSafe

    println("\nTest headOption:\n")
    print("foo.headOption = "); println(foo.headOption)
    print("baz.headOption = "); println(baz.headOption)
    print("bar.headOption = "); println(bar.headOption)

    println("\nTest headOption1:\n")
    print("foo.headOption1 = "); println(foo.headOption1)
    print("baz.headOption1 = "); println(baz.headOption1)
    print("bar.headOption1 = "); println(bar.headOption1)

    println("\nTest tailSafe:\n")
    print("foo.tailSafe = "); println(foo.tailSafe)
    print("baz.tailSafe = "); println(baz.tailSafe)
    print("bar.tailSafe = "); println(bar.tailSafe)

    // Test toList

    println("\nTest toList:\n")
    print("foo.toList = "); println(foo.toList)
    print("baz.toList = "); println(baz.toList)
    print("bar.toList = "); println(bar.toList)

    println("\nTest toList1:\n")
    print("foo.toList1 = "); println(foo.toList1)
    print("baz.toList1 = "); println(baz.toList1)
    print("bar.toList1 = "); println(bar.toList1)

    // Test drop and take

    println("\nTest drop and take:\n")
    val oneToTwenty = Stream(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
    print("oneToTwenty.drop(5).take(10).drop(2).toList = ")
    println(oneToTwenty.drop(5).take(10).drop(2).toList); println()

    val elevenToTwenty = oneToTwenty.drop(10)
    val elevenToFifteen = elevenToTwenty.take(5)
    val thirteenToFifteen = elevenToFifteen.drop(2)
    val stillThirteenToFifteen = thirteenToFifteen.take(999999999)
    val noneLeft = stillThirteenToFifteen.drop(999999999)
    print("elevenToTwenty.toList = "); println(elevenToTwenty.toList)
    print("elevenToFifteen.toList = "); println(elevenToFifteen.toList)
    print("thirteenToFifteen.toList = "); println(thirteenToFifteen.toList)
    print("stillThirteenToFifteen.toList = ")
    println(stillThirteenToFifteen.toList)
    print("noneLeft.toList = "); println(noneLeft.toList)

    // Test Laziness via a functions with side effects

    println("\nTest laziness with drop and take:\n")
    val putz = Stream.cons({print("three "); 3},
                 Stream.cons({print("two "); 2},
                   Stream.cons({print("one "); 1},
                     Stream.empty)))
    println("putz = Stream.cons({print(\"three \"); 3},")
    println("         Stream.cons({print(\"two \"); 2},")
    println("           Stream.cons({print(\"one \"); 1},")
    println("             Stream.empty)))\n")

    print("putz = "); println(putz)
    print("putz.drop(2).toList = "); println(putz.drop(2).toList)
    print("putz.drop(2).toList = "); println(putz.drop(2).toList)
    print("putz.take(1).toList = "); println(putz.take(1).toList)
    print("putz.take(1).toList = "); println(putz.take(1).toList)
    print("putz.toList = "); println(putz.toList)
    print("putz.toList = "); println(putz.toList)

    // Test takeWhile1 and dropWhile1 method

    println("\nTest dropWhile1 and takeWhile1:\n")

    val oneToTen = Stream(1,2,3,4,5,6,7,8,9,10)
    print("oneToTen.takeWhile1 (_ < 4).toList = ");
    println(oneToTen.takeWhile1(_ < 4).toList)
    print("oneToTen.dropWhile1(_ < 5).takeWhile1(_ < 8).toList = ");
    println(oneToTen.dropWhile1(_ < 5).takeWhile1(_ < 8).toList)

    val bad1To10 =
      Stream.cons({print("<1>"); 1},
        Stream.cons({print("<2>"); 2},
          Stream.cons({print("<3>"); 3},
            Stream.cons({print("<4>"); 4},
              Stream.cons({print("<5>"); 5},
                Stream.cons({print("<6>"); 6},
                  Stream.cons({print("<7>"); 7},
                    Stream.cons({print("<8>"); 8},
                      Stream.cons({print("<9>"); 9},
                        Stream.cons({print("<10>"); 10},
                          Stream.empty))))))))))
    println("\nbad1To10 = ")
    println("  Stream.cons({print(\"<1>\"); 1},")
    println("    Stream.cons({print(\"<2>\"); 2},")
    println("      Stream.cons({print(\"<3>\"); 3},")
    println("        Stream.cons({print(\"<4>\"); 4},")
    println("          Stream.cons({print(\"<5>\"); 5},")
    println("            Stream.cons({print(\"<6>\"); 6},")
    println("              Stream.cons({print(\"<7>\"); 7},")
    println("                Stream.cons({print(\"<8>\"); 8},")
    println("                  Stream.cons({print(\"<9>\"); 9},")
    println("                    Stream.cons({print(\"<10>\"); 10},")
    println("                      Stream.empty))))))))))")

    print("\nbadPlan = ")
    println("bad1To10.dropWhile1(_ < 3).takeWhile1(_ < 8)")

    val badPlan = bad1To10.dropWhile1(_ < 3).takeWhile1(_ < 8)

    print("\nbadPlan = "); println(badPlan)

    print("badPlan.toList = "); println(badPlan.toList)

    print("bad1To10.toList = "); println(bad1To10.toList)
    print("bad1To10.toList = "); println(bad1To10.toList)

    // Test exists & forAll (and indirectly foldRight)

    println("\nTest exists\n")

    print("3 and 5 are ")
    if (oneToTwenty exists ((b) => b % 3 == 0 && b % 5 == 0))
      println("factors of something in 1...20")
    else
      println("not factors of anything in 1...20")

    print("17 and 13 are ")
    if (oneToTwenty exists ((b) => b % 17 == 0 && b % 13 == 0))
      println("factors of something in 1...20")
    else
      println("not factors of anything in 1...20")

    println("\nTest exists1\n")

    print("3 and 5 are ")
    if (oneToTwenty exists1 ((b) => b % 3 == 0 && b % 5 == 0))
      println("factors of something in 1...20")
    else
      println("not factors of anything in 1...20")

    print("17 and 13 are ")
    if (oneToTwenty exists1 ((b) => b % 17 == 0 && b % 13 == 0))
      println("factors of something in 1...20.")
    else
      println("not factors of anything in 1...20")

    println("\nTest forAll\n")
    print("oneToTwenty forAll (_ < 10) = ")
    println(oneToTwenty forAll (_ < 10))
    print("oneToTwenty forAll (_ < 30) = ")
    println(oneToTwenty forAll (_ < 30))

    val bad1To6 =
      Stream.cons({print("<1>"); 1},
        Stream.cons({print("<2>"); 2},
          Stream.cons({print("<3>"); 3},
            Stream.cons({print("<4>"); 4},
              Stream.cons({print("<5>"); 5},
                Stream.cons({print("<6>"); 6},
                  Stream.empty))))))

    print("\nbad1To6 forAll (_ < 3) = ")
    println(bad1To6 forAll (_ < 3))
    print("bad1To6 forAll (_ < 10) = ")
    println(bad1To6 forAll (_ < 10))

    // Test takeWhile and dropWhile method

    println("\nTest dropWhile and takeWhile:\n")

    val oneToEleven = Stream(1,2,3,4,5,6,7,8,9,10,11)
    print("oneToEleven.takeWhile (_ < 4).toList = ");
    println(oneToEleven.takeWhile(_ < 4).toList)
    print("oneToEleven.dropWhile(_ < 5).takeWhile(_ < 8).toList = ");
    println(oneToEleven.dropWhile(_ < 5).takeWhile(_ < 8).toList)

    val bad1To11 =
      Stream.cons({print("<1>"); 1},
        Stream.cons({print("<2>"); 2},
          Stream.cons({print("<3>"); 3},
            Stream.cons({print("<4>"); 4},
              Stream.cons({print("<5>"); 5},
                Stream.cons({print("<6>"); 6},
                  Stream.cons({print("<7>"); 7},
                    Stream.cons({print("<8>"); 8},
                      Stream.cons({print("<9>"); 9},
                        Stream.cons({print("<10>"); 10},
                          Stream.cons({print("<11>"); 11},
                            Stream.empty)))))))))))
    println("\nbad1To11 = ")
    println("  Stream.cons({print(\"<1>\"); 1},")
    println("    Stream.cons({print(\"<2>\"); 2},")
    println("      Stream.cons({print(\"<3>\"); 3},")
    println("        Stream.cons({print(\"<4>\"); 4},")
    println("          Stream.cons({print(\"<5>\"); 5},")
    println("            Stream.cons({print(\"<6>\"); 6},")
    println("              Stream.cons({print(\"<7>\"); 7},")
    println("                Stream.cons({print(\"<8>\"); 8},")
    println("                  Stream.cons({print(\"<9>\"); 9},")
    println("                    Stream.cons({print(\"<10>\"); 10},")
    println("                      Stream.cons({print(\"<11>\"); 11},")
    println("                        Stream.empty)))))))))))")

    print("\nanotherBadPlan = ")
    println("bad1To11.dropWhile(_ < 3).takeWhile(_ < 8)")

    val anotherBadPlan = bad1To11.dropWhile(_ < 3).takeWhile(_ < 8)

    print("\nanotherBadPlan = "); println(anotherBadPlan)

    print("anotherBadPlan.toList = "); println(anotherBadPlan.toList)

    print("bad1To11.toList = "); println(bad1To11.toList)
    print("bad1To11.toList = "); println(bad1To11.toList)

    // Test map

    println("\nFirst test map using simple datastructures:\n")

    println("foo = Stream(2.1, 4.2, 5.3, 1.4, 8.5)")
    println("baz = Stream(42)")
    println("bar = Stream()\n")

    print("foo.map(_ + 1.0) = "); println(foo.map(_ + 1.0))
    print("foo.map(_ + 1.0).toList = "); println(foo.map(_ + 1.0).toList)
    print("baz.map(_ + 1.0).toList = "); println(baz.map(_ + 1.0).toList)
    print("bar.map(_ + 1.0).toList = "); println(bar.map(_ + 1.0).toList)

    println("\nTest laziness with an expensive function:\n")

    val domainFibs = Stream(0,1,2,3,4,5,6,7,8,9,10,
                             11,12,13,14,15,16,17,18,19,20,
                             21,22,23,24,25,26,27,28,29,30,
                             31,32,33,34,35,36,37,38,39,40,
                             41,42)
    val fibStream = domainFibs.map(fibPoor)
    val fibsTail = fibStream.drop(30)

    println("Start calculation:")
    print("fibTail.toList = "); println(fibsTail.toList)
    println("Finished calculation:\n")
    println("Start calculation:")
    print("fibStream.toList = "); println(fibStream.toList)
    println("Finished calculation:\n")

    println("\nTest laziness across streams:\n")

    println()

  }
}
