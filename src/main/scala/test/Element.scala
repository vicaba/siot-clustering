package test

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps

class Element(val position: Int, val value: Double, val addedFlexibleLoads: List[Double]) {

  def copy(
            position: Int = this.position,
            value: Double = this.value,
            addedFlexibleLoads: List[Double] = this.addedFlexibleLoads) = new Element(position, value, addedFlexibleLoads)

  override def toString: String =
    "E(" + value.toString + " -> " + (if (addedFlexibleLoads.isEmpty) "N" else addedFlexibleLoads) + ")"

  override def equals(obj: Any): Boolean = obj match {
    case e: Element => this.position == e.position
    case _ => false
  }

  override def hashCode(): Int = this.position

  def amplitude: Double = addedFlexibleLoads.foldLeft(this.value)(_ + _)

}

object Element {

  def toListOfElements(l: Vector[Double]): Vector[Element] = {
    l.zipWithIndex.map {case (e , i) => new Element(i, e, Nil)}
  }

  class ElementOrdering extends Ordering[Element] {
    override def compare(x: Element, y: Element): Int = implicitly[Ordering[Double]].compare(x.value, y.value)
  }

  implicit val elementOrdering: ElementOrdering = new ElementOrdering

  implicit val toVector: DenseVectorReprOps[Vector[Element]] = new DenseVectorReprOps[Vector[Element]] {

    override def apply(t: Vector[Element]): DenseVector[Double] = DenseVector(t.map(_.value):_*)

    override def zero(t: Vector[Element]): DenseVector[Double] = DenseVector((for(_ <- 1 to t.size) yield 0.0):_*)
  }

}
