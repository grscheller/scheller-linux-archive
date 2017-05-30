// Explore low level concurency mechanismism.

object LowlevelThreads {
  
  def main(args: Array[String]): Unit = {

    val hw =
      new Thread(
        new Runnable {
          def run() {
            println("hello world")
          }
        }
      )

    // Can only call Thread start method once.
    println("Now call hw.start")
    hw.start
    println("Just called hw.start")

  }

}
