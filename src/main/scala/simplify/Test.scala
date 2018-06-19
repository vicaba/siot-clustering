package simplify

object Test {

  case class Point(id: Int, other: Int = 2) {
    override def hashCode(): Int = id
  }

  def main(args: Array[String]): Unit = {
    val set = Set(Point(1), Point(2))
    println(set)
  }

}
