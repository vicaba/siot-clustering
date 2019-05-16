package test

class Element[E](val position: Int, val value: E, val approximateValue: Option[E]) {

  def copy(
            position: Int = this.position,
            value: E = this.value,
            approximateValue: Option[E] = this.approximateValue) = new Element(position, value, approximateValue)

  override def toString: String =
    "E(" + value.toString + " -> " + (if (approximateValue.isEmpty) "N" else approximateValue.get.toString) + ")"

}

object Element {

  def toListOfElements[T](l: Seq[T]): Seq[Element[T]] = {
    l.zipWithIndex.map {case (e , i) => new Element[T](i, e, None)}
  }

  private class ElementOrdering[T: Ordering] extends Ordering[Element[T]] {
    override def compare(x: Element[T], y: Element[T]): Int = implicitly[Ordering[T]].compare(x.value, y.value)
  }

  implicit def elementOrdering[E: Numeric]: ElementOrdering[E] = new ElementOrdering[E]

}
