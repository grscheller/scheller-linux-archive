package grokScala.codeblocks 

// Code blocks can have methods.

object CodeblocksContainingMethods {

  println("\nEnter constructor phase of CodeblocksContainingMethods object\n")

  // Opps, the lambda is the last thing returned,
  // λ-functions extend as far "to the left" as possible.
  val dog = {
    println("Initial woof.")
    def bark = println("woof woof")
    (numBarks: Int) => for (i <- 1 to numBarks) bark
    println("... and finally, woof!\n")
  }

  // Above made more clear.
  val leashedλdog = {
    println("Initial woof.")
    def bark = println("woof woof")
    (numBarks: Int) => {
       for (i <- 1 to numBarks) bark
       println("... and finally, woof!\n")
    }
  }

  def main(args: Array[String]) = {

    println("\nEntered main method\n")

    dog(5)
    dog(3)
    leashedλdog(2)
    leashedλdog(7)

    println("\nExiting main method\n")
  }

  println("\nExit constructor phase of CodeblocksContainingMethods object\n")
}
