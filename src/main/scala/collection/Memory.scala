package collection

trait Memory[T] {
  def areAllElementsEqual(): Boolean
  def +:(e: T): Memory[T]
}

object Memory {

  def apply[T](max: Int): Memory[T] = new MemoryImpl[T](Vector.empty, max)

  private[collection] class MemoryImpl[T](val list: Vector[T], val max: Int) extends Memory[T] {

    def areAllElementsEqual(): Boolean = if (list.size >= max) !list.exists(_ != list.head) else false

    def +:(e: T): Memory[T] = {
      val newList = if (list.size >= max) e +: list.dropRight(1) else e +: list

      new MemoryImpl[T](newList, max)
    }
  }
}