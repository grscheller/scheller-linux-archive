package grockScala.test.laziness

import grockScala.laziness._

object HasSubsequenceStreamTest{

  def main(args: Array[String]): Unit = {

    // Test zip
    println("\nTest zip method")
   
    println("Stream(1,2,3,4,5) zip Stream('a', 'b', 'c')")
    val foo = Stream(1,2,3,4,5) zip Stream('a', 'b', 'c')
    foo foreach println

    // Test startsWith and indirectly zipWith and zipAll
    println("\nTest startWith methods:")

    print("Stream.range(0,100,10) startsWith Stream.range(0,60,10)) = ")
    println(Stream.range(0,100,10) startsWith Stream.range(0,60,10))
    print("Stream.range(0,100,10) startsWith2 Stream.range(0,60,10)) = ")
    println(Stream.range(0,100,10) startsWith2 Stream.range(0,60,10))
    print("Stream.range(0,100,10) startsWith3 Stream.range(0,60,10)) = ")
    println(Stream.range(0,100,10) startsWith3 Stream.range(0,60,10))

    print("\nStream.range(0,100,10) startsWith Stream(1,2,3,4)) = ")
    println(Stream.range(0,100,10) startsWith Stream(1,2,3,4))
    print("Stream.range(0,100,10) startsWith2 Stream(1,2,3,4)) = ")
    println(Stream.range(0,100,10) startsWith2 Stream(1,2,3,4))
    print("Stream.range(0,100,10) startsWith3 Stream(1,2,3,4)) = ")
    println(Stream.range(0,100,10) startsWith3 Stream(1,2,3,4))

    print("\nStream.from(0) startsWith Stream.from(4)) = ")
    println(Stream.from(0) startsWith Stream.from(4))
    print("Stream.from(0) startsWith2 Stream.from(4)) = ")
    println(Stream.from(0) startsWith2 Stream.from(4))
    print("Stream.from(0) startsWith3 Stream.from(4)) = ")
    println(Stream.from(0) startsWith3 Stream.from(4))

    print("\nStream.from(0) startsWith Stream.range(0,50000)) = ")
    println(Stream.from(0) startsWith Stream.range(0,50000))
    print("Stream.from(0) startsWith2 Stream.range(0,1100)) = ")
    println(Stream.from(0) startsWith2 Stream.range(0,1100))
    print("Stream.from(0) startsWith3 Stream.range(0,1100)) = ")
    println(Stream.from(0) startsWith3 Stream.range(0,1100))

    print("\nStream.from(0) startsWith Stream.range(0,500)#:::Stream(3,2,1) = ")
    println(Stream.from(0) startsWith Stream.range(0,500)#:::Stream(3,2,1))
    print("Stream.from(0) startsWith2 Stream.range(0,500)#:::Stream(3,2,1) = ")
    println(Stream.from(0) startsWith2 Stream.range(0,500)#:::Stream(3,2,1))
    print("Stream.from(0) startsWith3 Stream.range(0,500)#:::Stream(3,2,1) = ")
    println(Stream.from(0) startsWith3 Stream.range(0,500)#:::Stream(3,2,1))

    print("\nStream(2,4,6,8) startsWith Stream(2,4,6,8,10,12)) = ")
    println(Stream(2,4,6,8) startsWith Stream(2,4,6,8,10,12))
    print("Stream(2,4,6,8) startsWith2 Stream(2,4,6,8,10,12)) = ")
    println(Stream(2,4,6,8) startsWith2 Stream(2,4,6,8,10,12))
    print("Stream(2,4,6,8) startsWith3 Stream(2,4,6,8,10,12)) = ")
    println(Stream(2,4,6,8) startsWith3 Stream(2,4,6,8,10,12))

    println()

  }
}
