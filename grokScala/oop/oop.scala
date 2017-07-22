package grokScala.oop 

import scala.util.{Try, Success, Failure}

// Interesting Scala factoids of an OOP nature.

// Class User illustrates what the existence of such a stupid
// construct as the null pointer does to bloat out code just to
// protect oneself.
class User {
  private var _name: String = ""
  def name = _name
  def name(userName: String) = {
    if (userName == null) 
      throw new NullPointerException("User.name must be non-null")
    _name = userName
  }
}

object User {
  def newUser(name: String): User = {
    val newUser = new User
    newUser.name(name)
    newUser
  }

}

object Main {

  def main(args: Array[String]) = {
    val bob = User.newUser("Bob")

    println(s"\nHello, my name is ${bob.name}.")
    bob.name("Robert")
    println(s"I changed my name to ${bob.name}.")

    // Check if a reference is just a reference
    val bobbie = bob
    bobbie.name("Bobbie")
    println(s"bobbie's name is ${bobbie.name}.")
    println(s"bob's name is ${bob.name}.\n")

    val sue = new User
    println(s"sue's name is the empty string: >>>${sue.name}<<<.")

    // sue has low self-esteem
    Try { sue.name(null) } match {
      case Success(_) => println(s"Sue's name is >>>${sue.name}<<<.")
      case Failure(e) => {
        println(e)
        print("Let's give her a nice name, ")
        sue.name("Susan")
        println(sue.name + ".\n")
      }
    }

    bob.name("Robert")
    println(s"Changed bob's name back to ${bob.name}.\n")
  }

}
