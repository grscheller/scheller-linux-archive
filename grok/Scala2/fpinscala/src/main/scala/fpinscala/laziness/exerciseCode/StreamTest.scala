package fpinscala.chap05.laziness

import fpinscala.laziness._
import fpinscala.laziness.Stream._

object StreamTest{

  // A really poor implementation for calculating fibonacci numbers.
  // To model an expensive calculation.
  def fibPoor(n: Int): Int =
    if (n < 2) n else fibPoor(n-1) + fibPoor(n-2)

  def main(args: Array[String]): Unit = {

    // Test the "constructor" in the companion object

    val foo = Stream(2.1, 4.2, 5.3, 1.4, 8.5)
    val baz = Stream(42)
    val bar = Stream[Double]()

    println("\nDatastructures used:\n")
    println("foo = Stream(2.1, 4.2, 5.3, 1.4, 8.5)")
    println("baz = Stream(42)")
    println("bar = Stream[Double]()")

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
    val putz = cons({print("three "); 3},
                 cons({print("two "); 2},
                   cons({print("one "); 1},
                     empty)))
    println("putz = cons({print(\"three \"); 3},")
    println("         cons({print(\"two \"); 2},")
    println("           cons({print(\"one \"); 1},")
    println("             empty)))\n")

    print("putz = "); println(putz)
    print("putz.drop(2).toList = "); println(putz.drop(2).toList)
    print("putz.drop(2).toList = "); println(putz.drop(2).toList)
    print("putz.take(1).toList = "); println(putz.take(1).toList)
    print("putz.take(1).toList = "); println(putz.take(1).toList)
    print("putz.toList = "); println(putz.toList)
    print("putz.toList = "); println(putz.toList)

    // Test takeWhile1 and dropWhile method

    println("\nTest dropWhile and takeWhile1:\n")

    val oneToTen = Stream(1,2,3,4,5,6,7,8,9,10)
    print("oneToTen.takeWhile1 (_ < 4).toList = ");
    println(oneToTen.takeWhile1(_ < 4).toList)
    print("oneToTen.dropWhile(_ < 5).takeWhile1(_ < 8).toList = ");
    println(oneToTen.dropWhile(_ < 5).takeWhile1(_ < 8).toList)

    val bad1To10 =
      cons({print("<1>"); 1},
        cons({print("<2>"); 2},
          cons({print("<3>"); 3},
            cons({print("<4>"); 4},
              cons({print("<5>"); 5},
                cons({print("<6>"); 6},
                  cons({print("<7>"); 7},
                    cons({print("<8>"); 8},
                      cons({print("<9>"); 9},
                        cons({print("<10>"); 10},
                          empty))))))))))
    println("\nbad1To10 = ")
    println("  cons({print(\"<1>\"); 1},")
    println("    cons({print(\"<2>\"); 2},")
    println("      cons({print(\"<3>\"); 3},")
    println("        cons({print(\"<4>\"); 4},")
    println("          cons({print(\"<5>\"); 5},")
    println("            cons({print(\"<6>\"); 6},")
    println("              cons({print(\"<7>\"); 7},")
    println("                cons({print(\"<8>\"); 8},")
    println("                  cons({print(\"<9>\"); 9},")
    println("                    cons({print(\"<10>\"); 10},")
    println("                      empty))))))))))")

    print("\nset badPlan = ")
    println("bad1To10.dropWhile(_ < 3).takeWhile1(_ < 8)")

    println("Make badPlan")
    val badPlan = bad1To10.dropWhile(_ < 3).takeWhile1(_ < 8)
    println("\nFinished making badPlan")

    // If I imported the Traversable trait,
    // below println will force an evaluation.
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
      cons({print("<1>"); 1},
        cons({print("<2>"); 2},
          cons({print("<3>"); 3},
            cons({print("<4>"); 4},
              cons({print("<5>"); 5},
                cons({print("<6>"); 6},
                  empty))))))

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
      cons({print("<1>"); 1},
        cons({print("<2>"); 2},
          cons({print("<3>"); 3},
            cons({print("<4>"); 4},
              cons({print("<5>"); 5},
                cons({print("<6>"); 6},
                  cons({print("<7>"); 7},
                    cons({print("<8>"); 8},
                      cons({print("<9>"); 9},
                        cons({print("<10>"); 10},
                          cons({print("<11>"); 11},
                            empty)))))))))))
    println("\nbad1To11 = ")
    println("  cons({print(\"<1>\"); 1},")
    println("    cons({print(\"<2>\"); 2},")
    println("      cons({print(\"<3>\"); 3},")
    println("        cons({print(\"<4>\"); 4},")
    println("          cons({print(\"<5>\"); 5},")
    println("            cons({print(\"<6>\"); 6},")
    println("              cons({print(\"<7>\"); 7},")
    println("                cons({print(\"<8>\"); 8},")
    println("                  cons({print(\"<9>\"); 9},")
    println("                    cons({print(\"<10>\"); 10},")
    println("                      cons({print(\"<11>\"); 11},")
    println("                        empty)))))))))))")

    print("\nset anotherBadPlan = ")
    println("bad1To11.dropWhile(_ < 3).takeWhile(_ < 8)")

    println("Make anotherBadPlan")
    val anotherBadPlan = bad1To11.dropWhile(_ < 3).takeWhile(_ < 8)
    println("\nFinished making anotherBadPlan")

    print("\nanotherBadPlan = "); println(anotherBadPlan)

    print("anotherBadPlan.toList = "); println(anotherBadPlan.toList)

    print("bad1To11.toList = "); println(bad1To11.toList)
    print("bad1To11.toList = "); println(bad1To11.toList)

    // Test map

    println("\nFirst test map using simple datastructures:\n")

    println("foo = Stream(2.1, 4.2, 5.3, 1.4, 8.5)")
    println("baz = Stream(42)")
    println("bar = Stream[Double]()\n")

    // I think I am dragging in some _.toString method
    // which is forcing an evaluation from Traversable trait?
    print("foo.map(_ + 1.0) = ")
    println(foo.map(_ + 1.0))
    print("foo.map(_ + 1.0).toList = ")
    println(foo.map(_ + 1.0).toList)
    print("baz.map(_ + 1.0).toList = ")
    println(baz.map(_ + 1.0).toList)
    print("bar.map(_ + 1.0).toList = ")
    println(bar.map(_ + 1.0).toList)

    println("\nTest laziness with an expensive function:\n")

    val domainFibs = Stream(0,1,2,3,4,5,6,7,8,9,10,
                             11,12,13,14,15,16,17,18,19,20,
                             21,22,23,24,25,26,27,28,29,30,
                             31,32,33,34,35,36,37,38,39,40)
    val fibStream = domainFibs.map(fibPoor)
    val fibsTail = fibStream.drop(30)

    println("Start calculation:")
    print("fibTail.toList = "); println(fibsTail.toList)
    println("Finished calculation:\n")
    println("Start calculation:")
    print("fibStream.toList = "); println(fibStream.toList)
    println("Finished calculation:\n")

    println("\nTest both prepends(#:: & #:::) and append(:::#):")

    val bad1To5 =
      cons({print("<1>"); 1},
        cons({print("<2>"); 2},
          cons({print("<3>"); 3},
            cons({print("<4>"); 4},
              cons({print("<5>"); 5},
                empty)))))

    print("\nMake bad0To5")
    val bad0To5 = {print("<0>"); 0} #:: bad1To5
    print("\nMake bad42and0To5")
    val bad42and0To5 = cons({print("<42>"); 42}, bad0To5);
    println()

    print("bad42and0To5.toList = "); println(bad42and0To5.toList)

    val bad1To8 =
      cons({print("<1>"); 1},
        cons({print("<2>"); 2},
          cons({print("<3>"); 3},
            cons({print("<4>"); 4},
              cons({print("<5>"); 5},
                cons({print("<6>"); 6},
                  cons({print("<7>"); 7},
                    cons({print("<8>"); 8},
                      empty))))))))

    print("\nMake bad567")
    val bad567 = bad1To8.drop(4).take(3)
    print("\nMake bad123456")
    val bad123456 = bad1To8.take(6)
    print("\nMake bad567123456")
    val bad567123456 = bad567 #::: bad123456
    print("\nMake bad12345678567")
    val bad12345678567 = bad1To8 :::# bad567
    println()

    print("bad567123456.toList = "); println(bad567123456.toList)
    print("bad12345678567.toList = "); println(bad12345678567.toList)

    // Compare flatMap1 and flatMap

    println("\nCompare flatMap1 and flatMap with some evaluated data:\n")

    val numStrings = Stream("4", "0", "1", "bob", "2", "3")

    def numStringToCharStream(s: String): Stream[Char] = {
      val sAsInt =
        try {
            s.toInt
        } catch {
            case e: Exception => 0
        }

      var outStream: Stream[Char] = empty
      for (ii <- 1 to sAsInt) {
        val tempStream = outStream  // Needed to avoid infinite datastructure
        outStream = cons(('`' + sAsInt).toChar, tempStream)
      }

      outStream

    }

    print("(numStrings flatMap1 numStringToCharStream).toList = ")
    println((numStrings flatMap1 numStringToCharStream).toList)

    print("(numStrings flatMap numStringToCharStream).toList = ")
    println((numStrings flatMap numStringToCharStream).toList)

    println("\nCompare flatMap1 and flatMap with unevaluated data:\n")

    val unEval1 =
      cons({print("<4>"); "4"},
        cons({print("<0>"); "0"},
          cons({print("<1>"); "1"},
            cons({print("<bob>"); "bob"},
              cons({print("<2>"); "2"},
                cons({print("<3>"); "3"},
                  empty))))))
    val unEval2 =
      cons({print("<4>"); "4"},
        cons({print("<0>"); "0"},
          cons({print("<1>"); "1"},
            cons({print("<bob>"); "bob"},
              cons({print("<2>"); "2"},
                cons({print("<3>"); "3"},
                  empty))))))

    print("(unEval1 flatMap1 numStringToCharStream).toList = ")
    println((unEval1 flatMap1 numStringToCharStream).toList)

    print("(unEval2 flatMap numStringToCharStream).toList = ")
    println((unEval2 flatMap numStringToCharStream).toList)

    println("\nTest flatMap via for comprehension:\n")

    val evaluatedStream = unEval1 #::: unEval2.takeWhile(_.length < 3)
    val charStream =
      for {
        numStr <- evaluatedStream
        char <- numStringToCharStream(numStr)
      } yield char

    print("charStream.toList = ")
    println(charStream.toList)

    // For testing covariance and contravariance.
    println("\nTest variance of #:::, :::#, and flatMaps:\n")

    abstract class Fruit { print("<yum>") }
    class Apple extends Fruit { print("<crisp>") }  // purposely not case class to
    class Orange extends Fruit { print("<juicy>") } // test how well matching works
    class Pear extends Fruit { print("<eve's real favorite>") }
    class Kiwi extends Fruit { print("<exotic>") }
    class Pineapple extends Fruit { print("<sweet>") }

    println("Make apples stream")
    val apples = Stream(new Apple, new Apple, new Apple)
    println("\nMake orange stream")
    val oranges = Stream(new Orange, new Orange)
    println("\nMake fruit1 stream")
    val fruit1 = apples #::: oranges
    println("Make fruit2 stream reuse some fruit")
    val fruit2 = apples :::# oranges :::# Stream(new Pineapple, new Pear)

    print("\nfruit1.toList = "); println(fruit1.toList)
    print("\nfruit2.toList = "); println(fruit2.toList)

    print("\nfruit1 == fruit2 = ")
    println(fruit1 == fruit2)
    print("\nfruit1.toList == fruit2.toList = ")
    println(fruit1.toList == fruit2.toList)

    print("\nfruit1 == fruit1 = ")
    println(fruit1 == fruit1)
    print("\nfruit2.toList == fruit2.toList = ")
    println(fruit2.toList == fruit2.toList)

    print("\nfruit1  = "); println(fruit1)
    print("\nfruit2  = "); println(fruit2)

    val fruit3      = cons( new Pineapple
                          , cons( new Apple
                          , cons( new Pear
                          , empty[Fruit])))

    val fruit3Clone = cons( new Pineapple
                          , cons( new Apple
                          , cons( new Pear
                          , empty[Fruit])))

    print("\nfruit3 == fruit3 = ")
    println(fruit3 == fruit3)
    print("\nfruit3 == fruit3Clone = ")
    println(fruit3 == fruit3Clone)
    print("\nfruit3.toList == fruit3Clone.toList = ")
    println(fruit3.toList == fruit3Clone.toList)

    println("\nMake fruit stream")
    val fruit4 = Stream( new Apple, new Orange
                       , new Apple, new Pear
                       , new Kiwi )
    print("\nfruit4.toList = "); println(fruit4.toList)

    def printFruitStreamVersions(fruits: Stream[Fruit]) =
      for (fruit <- fruits) {
        fruit match {
          case _: Apple     => println("An apple")
          case _: Orange    => println("An orange")
          case _: Pear      => println("A pear")
          case _: Pineapple => println("A pineapple")
          case _: Fruit     => println("A fruit")
          case _            => println("Something else")
        }
      }

    println("\nPrint out fruit versions in fruit4")
    printFruitStreamVersions(fruit4)

    // Make some lazy fruit
    println("\nMake some bad lazy fruit\n")
    val badLazyFruit =
      cons({print("<1>"); new Orange},
        cons({print("<2>"); new Apple},
          cons({print("<3>"); new Apple},
            cons({print("<4>"); new Pear},
              cons({print("<5>"); new Orange},
                cons({print("<6>"); new Kiwi},
                  empty))))))

    println("Print out fruit in badLazyFruit")
    printFruitStreamVersions(badLazyFruit)

    // Test find
    println("\nTest find with evaluated fruit:")

    val maybeApple = badLazyFruit.find(_ match {
      case _: Apple => true
      case _ => false
    })
    val maybePear = badLazyFruit.find(_ match {
      case _: Pear => true
      case _ => false
    })
    val maybePineapple = badLazyFruit.find(_ match {
      case a: Pineapple => true
      case _ => false
    })
    val maybeKiwi = badLazyFruit.find(_ match {
      case a: Kiwi => true
      case _ => false
    })
    val streamOfMaybeFruit = Stream( maybeApple
                                   , maybePear
                                   , maybePineapple
                                   , maybeKiwi )
    printFruitStreamVersions(streamOfMaybeFruit filter {
        case Some(_) => true
        case _ => false
      } map (_.get))

    println()

  }
}
