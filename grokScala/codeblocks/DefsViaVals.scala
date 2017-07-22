package grokScala.codeblocks 

// defs and vals are really quite similar, in Scala they
// share the same namespace and I can implement "defs"
// in terms of vals of codeblocks returning lambdas.

object DefsViaVals {

  def object0Def = {
    print("<object0Def enter>")
    42
  }

  val object0Val = {
    print("<object0Val enter>")
    42
  }

  lazy val object0LazyVal = {
    print("<object0LazyVal enter>")
    42
  }

  def object1Def(a: Int) = {
    print("<object1Def enter>")
    a + 1
  }

  def object1Val = {
    print("<object1Val enter>")
    (a: Int) => a + 1
  }

  def main(args: Array[String]) = {

    println("\n\nEntered main method")

    def main0Def = {
      print("<main0Def enter>")
      42
    }

    val main0Val = {
      print("<main0Val enter>")
      42
    }

    lazy val main0LazyVal = {
      print("<main0LazyVal enter>")
      42
    }

    def main1Def(a: Int) = {
      print("<main1Def enter>")
      a + 1
    }

    def main1Val = {
      print("<main1Val enter>")
      (a: Int) => a + 1
    }

    // Now test them:

    print("\n\nmain0Def = "); println(main0Def)
    print("object0Def = "); println(object0Def)
    print("main1Def(10) = "); println(main1Def(10))
    print("object1Def(10) = "); println(object1Def(10))

    print("\nmain0Val = "); println(main0Val)
    print("object0Val = "); println(object0Val)
    print("main0LazyVal = "); println(main0LazyVal)
    print("object0LazyVal = "); println(object0LazyVal)
    print("main0LazyVal = "); println(main0LazyVal)
    print("object0LazyVal = "); println(object0LazyVal)
    print("main1Val(10) = "); println(main1Val(10))
    print("object1Val(10) = "); println(object1Val(10))

    println()
  }

}
