package test

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps

class Element[E](val position: Int, val value: E, val approximateValue: Option[E]) {

  def copy(
            position: Int = this.position,
            value: E = this.value,
            approximateValue: Option[E] = this.approximateValue) = new Element(position, value, approximateValue)

  override def toString: String =
    "E(" + value.toString + " -> " + (if (approximateValue.isEmpty) "N" else approximateValue.get.toString) + ")"

  override def equals(obj: Any): Boolean = obj match {
    case e: Element[E] => this.position == e.position && this.value.getClass == e.value.getClass
    case _ => false
  }

  override def hashCode(): Int = this.position

}

object Element {

  def toListOfElements[T](l: Vector[T]): Vector[Element[T]] = {
    l.zipWithIndex.map {case (e , i) => new Element[T](i, e, None)}
  }

  class ElementOrdering[T: Ordering] extends Ordering[Element[T]] {
    override def compare(x: Element[T], y: Element[T]): Int = implicitly[Ordering[T]].compare(x.value, y.value)
  }

  implicit def elementOrdering[E: Numeric]: ElementOrdering[E] = new ElementOrdering[E]

  implicit val toVector: DenseVectorReprOps[Vector[Element[Double]]] = new DenseVectorReprOps[Vector[Element[Double]]] {

    override def apply(t: Vector[Element[Double]]): DenseVector[Double] = DenseVector(t.map(_.value):_*)

    override def zero(t: Vector[Element[Double]]): DenseVector[Double] = DenseVector((for(_ <- 1 to t.size) yield 0.0):_*)
  }

}
