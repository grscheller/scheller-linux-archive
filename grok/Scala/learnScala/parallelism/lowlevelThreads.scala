// Explore low level concurency mechanismism.

package grokScala.parallelism

object LowlevelThreads {
  
  def main(args: Array[String]): Unit = {

    println("entering main")

    val helloThread =
      new Thread(
        new Runnable {
          def run() {
            println("hello from thread")
            Thread.sleep(1000)
            println("so long from thread")
          }
        }
      )

    // Can only call Thread start method once.
    println("now call helloThread.start")
    helloThread.start
    println("just called helloThread.start")
    Thread.sleep(500)
    println("exiting main")

  }

}
