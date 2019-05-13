package test

import breeze.linalg._
import scala.language.implicitConversions

object Main {

  def toListOfElements[T: Numeric](l: List[T]): List[Element[T]] =
    l.zipWithIndex.map {case (e , i) => new Element[T](i, e, None)}

  class Element[E: Numeric](val position: Int, val value: E, val approximateValue: Option[E]) {

    def copy(
              position: Int = this.position,
              value: E = this.value,
              approximateValue: Option[E] = this.approximateValue) = new Element(position, value, approximateValue)

    //override def toString: String = "Element("+ position.toString + ", " + value.toString + ", " + approximateValue.toString + ")"

    override def toString: String =
      "E(" + value.toString + " -> " + (if (approximateValue.isEmpty) "N" else approximateValue.get.toString) + ")"

  }


  def main(args: Array[String]): Unit = {
/*    LineChartSample.start(
      data = List[(Number, Number)](
      (1, 23),
      (2, 27),
      (3, 15),
      (4, 24),
      (5, 34),
      (6, 36),
      (7, 22),
      (8, 45)
    ))*/
    approximateMethod()
  }

  def approximateMethod(): Unit = {

    //println(findClosest(3.5, toListOfElements(List(1.0, 2.0, 3.0, 4.0, 5.0))))

    val input = List(1.7, 3.0, 2.0, 4.0, 5.0).reverse

    val output = sortByApproximation(input, List(1.0, 2.0, 3.0, 4.0, 5.0))

    println(input)
    println(output)

  }

  def findClosest(originalPoint: Double, listOfPoints: List[Element[Double]]): List[(Double, Element[Double])] =
    listOfPoints map { p => (norm(DenseVector(originalPoint) - DenseVector(p.value), 2), p) } sortBy(_._1)

  def sortByApproximation(toApproximate: List[Double], fixed: List[Double]): List[Element[Double]] = {

    def _sortByApproximation(toApproximate: List[Double], _fixed: List[Element[Double]]): List[Element[Double]] = {
      toApproximate match {
        case e :: es =>
          val cl = findClosest(e, _fixed.filter(_.approximateValue.isEmpty))
          val newValue = cl.head._2.copy(approximateValue = Some(e))
          _sortByApproximation(es, _fixed.updated(newValue.position, newValue))
        case Nil => _fixed

      }
    }
    _sortByApproximation(toApproximate, toListOfElements(fixed))
  }

}
