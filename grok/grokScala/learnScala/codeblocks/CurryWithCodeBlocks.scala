package grokScala.codeblocks 

// Currying with code blocks.
// Both cb and lamb are of type
//   Int = (Int => Int) = <function1>

object CurryWithCodeBlocks {

  def main(args: Array[String]) = {

    // Implement a "Lambda" with a code block:
    val cb = {
      val secretNumber = 10
      var statefulNumber = 0
      (a: Int) => statefulNumber = a * secretNumber
      (b: Int) => statefulNumber + b
    }

    // Real Lambda:
    val lamb = (a: Int) => (b: Int) => a*10 + b

    println("\nExecute cb(4)(2):")
    val resultCB = cb(4)(2)
    println(s"result = ${resultCB}")

    println("\nExecute lamb(4)(2):")
    val resultLambda = lamb(4)(2)
    println(s"result = ${resultLambda}")

    println("\nPartially apply -> bar = cb(4)")
    val bar = cb(4)
    println("Finish applying -> bar(2)")
    val resultBar = bar(2)
    println(s"result = ${resultBar}\n")

  }

}
