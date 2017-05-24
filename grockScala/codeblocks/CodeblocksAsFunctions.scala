package grockScala.codeblocks 

// Having a single Lambda anywhere in a code block
// turns it into a function1.

object CodeblocksAsFunctions {

  // Except for some newlines
  println("\n<Side effects are inclosed in <> >")

  // See if what I did for REPL repeats for compiled code.
  val dog = {
    print("\n<Entered dog code block>")
    var fido = 7
    (a: Int) => fido = fido + a
    print("<fido is " + fido + ">")
  }

  // See if it is the Lambda doing it.
  val cat = {
    print("\n<Entered cat code block>")
    val fifi = 9
    (a: Int) => fifi + a
    print("<fifi is " + fifi + ">")
  }

  // See if it is the var doing it.
  val goat = {
    print("\n<Entered goat code block>")
    var billy = 10
    val bg = (a: Int) => billy + a
    billy = bg(5)
    print("<billy is " + billy + ">")
  }

  def main(args: Array[String]) = {

    println("\n\nEntered main method")

    // See if what I did for REPL repeats for compiled code.
    val mainDog = {
      print("\n<Entered mainDog code block>")
      var rover = 5
      (a: Int) => rover = rover + a
      print("<rover is " + rover + ">")
    }

    // Shows that it is the Lambda doing it.
    val mainCat = {
      print("\n<Entered mainCat code block>")
      val fifi = 9
      (a: Int) => fifi + a
      print("<fifi is " + fifi + ">")
    }

    // Just having a var in the code block does not do it.
    val mainGoat = {
      print("\n<Entered mainGoat code block>")
      var nanny = 10
      val ng = (a: Int) => nanny + a
      nanny = ng(5)
      print("<nanny is " + ng(nanny) + ">")
    }

    // Now test them:

    print("\n\ndog(10) = "); println(dog(10))
    print("mainDog(10) = "); println(mainDog(10))
    print("\ncat(10) = "); println(cat(10))
    print("mainCat(10) = "); println(mainCat(10))
    print("\ngoat = "); println(goat)
    print("mainGoat = "); println(mainGoat)

  }

}
