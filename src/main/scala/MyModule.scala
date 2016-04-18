// Comment - start off with something familiar to OO programmer
/*
   Longer Multi-line comment

   Using the MyModule "singular" object as
   a mamespace.  

*/
/** Documentation comment for MyModule */
object MyModule {

  // Note that if ... else ... is a an expression
  //   and returns a value, Int in this case, but
  //   could be (): Unit as main does.
  // Body of this function is a single expression. 
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  // The return type is String and is being
  //   inferred by the scala compiler.  The
  //   return types on abs and main methods
  //   are redundant.
  // Calls the format method of String class.
  // Functions/methods return the last expression
  //  in the {} expresion block.
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  // This method is not "pure" because it 
  //   has a side effect - it changes the state of
  //   my videomonitor.
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

}
