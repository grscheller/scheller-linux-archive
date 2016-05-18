package grockScala.test.laziness

import grockScala.laziness._

object StreamTest{

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

    println("\nTest tailSafe:\n")
    print("foo.tailSafe = "); println(foo.tailSafe)
    print("baz.tailSafe = "); println(baz.tailSafe)
    print("bar.tailSafe = "); println(bar.tailSafe)

    // Test toList

    println("\nTest toList:\n")
    print("foo.toList = "); println(foo.toList)
    print("baz.toList = "); println(baz.toList)
    print("bar.toList = "); println(bar.toList)

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

    // Test takeWhile method

    println("\nTest dropWhile and takeWhile:\n")

    val oneToTen = Stream(1,2,3,4,5,6,7,8,9,10)
    print("oneToTen.takeWhile(_ < 4).toList = ");
    println(oneToTen.takeWhile(_ < 4).toList)
    print("oneToTen.dropWhile(_ < 5).takeWhile(_ < 8).toList = ");
    println(oneToTen.dropWhile(_ < 5).takeWhile(_ < 8).toList)

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
    println("bad1To10.dropWhile(_ < 3).takeWhile(_ < 8)")

    val badPlan = bad1To10.dropWhile(_ < 3).takeWhile(_ < 8)

    print("\nbadPlan = "); println(badPlan)

    print("badPlan.toList = "); println(badPlan.toList)

    print("bad1To10.toList = "); println(bad1To10.toList)

    println()

  }
}
