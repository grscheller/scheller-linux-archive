package typeLevelProgramming

object TypeLevelProgramming {
  import scala.reflect.runtime.universe._
  def show[T](value: T)(implicit tag: TypeTag[T]) =
    tag.toString.replace("typeLevelProgramming.TypeLevelProgramming.", "")

  def main(args: Array[String]): Unit = {
    println("This is where main will live")
  }

}
