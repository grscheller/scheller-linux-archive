package methodAsFunctions

class MethodAsFunctions {
  def m1(x: Int) = x + 30        // instance method
  val f1 = (x: Int) => x + 30    // λ-function
  val f2 = m1 _                  // function object ???
  val f3: (Int) => Int = m1      // function object ???
}

object MethodAsFunctions {
  def main(args: Array[String]) = {

    val maf = new MethodAsFunctions

    println(s"maf.m1(12) = ${maf.m1(12)}")
    println(s"maf.f1(12) = ${maf.f1(12)}")
    println(s"maf.f2(12) = ${maf.f2(12)}")
    println(s"maf.f3(12) = ${maf.f3(12)}")

  }
}
