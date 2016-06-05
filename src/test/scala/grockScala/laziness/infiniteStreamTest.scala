package grockScala.test.laziness

import grockScala.laziness._

object infiniteStreamTest{

  // Infinite data structure - infinite Stream of 42's
  val fortyTwos: Stream[Int] = Stream.cons(42, fortyTwos)

  def main(args: Array[String]): Unit = {

    // Compare member infinite vals vs. local vals

    print("fortyTwos.drop(42).take(42).headOption = ")
    println(fortyTwos.drop(42).take(42).headOption)

    /*
       Does not compile.  Error message is:
         "forward reference extends over
          definition of value ones"
       Also fails with Standard Scala Stream too.
    */
    // val ones: Stream[Int] = Stream.cons(1, ones)
    // print("ones.drop(42).take(10).headOption = ")
    // println(ones.drop(42).take(10).headOption)

    println()

  }
}
