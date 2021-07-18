package fpinscala.chap05.laziness

import fpinscala.laziness._
import fpinscala.laziness.Stream._

object hasSubsequenceStreamTest {

  def main(args: Array[String]): Unit = {

    // Test zip
    println("\nTest zip method")

    println("Stream(1,2,3,4,5) zip Stream('a', 'b', 'c')")
    val foo = Stream(1, 2, 3, 4, 5) zip Stream('a', 'b', 'c')
    foo foreach println

    // Test startsWith and indirectly zipWith and zipAll
    println("\nTest startWith methods:")

    print("range(0,100,10) startsWith range(0,60,10)) = ")
    println(range(0, 100, 10) startsWith range(0, 60, 10))
    print("range(0,100,10) startsWith2 range(0,60,10)) = ")
    println(range(0, 100, 10) startsWith2 range(0, 60, 10))
    print("range(0,100,10) startsWith3 range(0,60,10)) = ")
    println(range(0, 100, 10) startsWith3 range(0, 60, 10))

    print("\nrange(0,100,10) startsWith Stream(1,2,3,4)) = ")
    println(range(0, 100, 10) startsWith Stream(1, 2, 3, 4))
    print("range(0,100,10) startsWith2 Stream(1,2,3,4)) = ")
    println(range(0, 100, 10) startsWith2 Stream(1, 2, 3, 4))
    print("range(0,100,10) startsWith3 Stream(1,2,3,4)) = ")
    println(range(0, 100, 10) startsWith3 Stream(1, 2, 3, 4))

    print("\nfrom(0) startsWith from(4)) = ")
    println(from(0) startsWith from(4))
    print("from(0) startsWith2 from(4)) = ")
    println(from(0) startsWith2 from(4))
    print("from(0) startsWith3 from(4)) = ")
    println(from(0) startsWith3 from(4))

    print("\nfrom(0) startsWith range(0,50000)) = ")
    println(from(0) startsWith range(0, 50000))
    print("from(0) startsWith2 range(0,900)) = ")
    println(from(0) startsWith2 range(0, 900))
    print("from(0) startsWith3 range(0,900)) = ")
    println(from(0) startsWith3 range(0, 900))

    print("\nfrom(0) startsWith range(0,500)#:::Stream(3,2,1) = ")
    println(from(0) startsWith range(0, 500) #::: Stream(3, 2, 1))
    print("from(0) startsWith2 range(0,500)#:::Stream(3,2,1) = ")
    println(from(0) startsWith2 range(0, 500) #::: Stream(3, 2, 1))
    print("from(0) startsWith3 range(0,500)#:::Stream(3,2,1) = ")
    println(from(0) startsWith3 range(0, 500) #::: Stream(3, 2, 1))

    print("\nStream(2,4,6,8) startsWith Stream(2,4,6,8,10,12)) = ")
    println(Stream(2, 4, 6, 8) startsWith Stream(2, 4, 6, 8, 10, 12))
    print("Stream(2,4,6,8) startsWith2 Stream(2,4,6,8,10,12)) = ")
    println(Stream(2, 4, 6, 8) startsWith2 Stream(2, 4, 6, 8, 10, 12))
    print("Stream(2,4,6,8) startsWith3 Stream(2,4,6,8,10,12)) = ")
    println(Stream(2, 4, 6, 8) startsWith3 Stream(2, 4, 6, 8, 10, 12))

    // Test tails1 and tails methods

    println("\nTest tails1 and tails methods")

    println("\nStream(1,2,3,4).tails1 map (_.toList) foreach println = ")
    Stream(1, 2, 3, 4).tails1 map (_.toList) foreach println

    println("\nStream[Double]().tails1 map (_.toList) foreach println = ")
    Stream[Double]().tails1 map (_.toList) foreach println

    println("\nStream(1,2,3,4).tails map (_.toList) foreach println = ")
    Stream(1, 2, 3, 4).tails map (_.toList) foreach println

    println("\nStream[Double]().tails map (_.toList) foreach println = ")
    Stream[Double]().tails map (_.toList) foreach println

    // Test hasSubsequence

    println("\nTest hasSubsequence method")

    print("\nrange(1,1000) hasSubsequence ")
    print("range(875, 889) = ")
    println(range(1, 1000) hasSubsequence range(875, 889))

    print("\nrange(1,1000) hasSubsequence ")
    print("Stream(5,4,3,2,1) = ")
    println(range(1, 1000) hasSubsequence Stream(5, 4, 3, 2, 1))

    print("\nrange(1,1000) hasSubsequence ")
    print("Stream(999,1000,1001) = ")
    println(range(1, 1000) hasSubsequence Stream(999, 1000, 1001))

    println()

  }
}
