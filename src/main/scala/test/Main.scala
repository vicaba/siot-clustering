package test

import breeze.linalg._
import types.{DataTypeMetadata, DataTypeMetadata2Columns}
import types.immutable.Point
import types.mutable.Cluster
import types.ops.MirrorImage.MirroredSyntheticDataType

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
    approximateMethod()
    //clusterApproximateMehod()
  }

  def clusterApproximateMehod(): Unit = {
    implicit val types: DataTypeMetadata = DataTypeMetadata2Columns

    val allPoints = List(
      DenseMatrix((5.0, 7.0), (5.0, 7.0)),
      DenseMatrix((7.0, 10.0), (6.0, 7.0))
    ).zipWithIndex.map {
      case (m, idx) =>
        Point(idx, m, None).toCluster
    }.toVector

    val fixedPoints = List(
      DenseMatrix((5.0, 7.0)),
      DenseMatrix((7.0, 10.0))
    ).zipWithIndex.map {
      case (m, idx) =>
        Point(idx, m, None).toCluster
    }.toVector

    val flexiblePoints = List(
      DenseMatrix((5.0, 7.0)),
      DenseMatrix((6.0, 7.0))
    ).zipWithIndex.map {
      case (m, idx) =>
        Point(idx, m, None).toCluster
    }.toVector

    val clusterAllPoints = Cluster(1, "ClusterAllPoints", allPoints, 1, None)
    val clusterFixedPoints = Cluster(2, "ClusterFixedPoints", fixedPoints, 1, None)
    val clusterFlexiblePoints = Cluster(3, "ClusterFlexiblePoints", flexiblePoints, 1, None)


    val averageData = for (_ <- 1 to 2) yield 0.0

    val averageVector = DenseVector(averageData: _*)

    val mirror = MirroredSyntheticDataType.findMirror(clusterFixedPoints.syntheticValue, averageVector)

    val fixedPointsToChart = clusterFixedPoints.centroid.toArray.toList.zipWithIndex.map(t => (t._2, t._1).asInstanceOf[(Number, Number)])

    val flexiblePointsToChart = clusterFlexiblePoints.centroid.toArray.toList.zipWithIndex.map(t => (t._2, t._1).asInstanceOf[(Number, Number)])

    val rescheduledFlexiblePoints = List(
      DenseMatrix(sortDenseMatrixByApproximation(clusterFlexiblePoints.points.head.data, mirror).map(_.approximateValue.get)),
      DenseMatrix(sortDenseMatrixByApproximation(clusterFlexiblePoints.points.last.data, mirror).map(_.approximateValue.get))
    ).zipWithIndex.map {
      case (m, idx) =>
        Point(idx, m, None).toCluster
    }.toVector

    val clusterRescheduledFlexiblePoints = Cluster(4, "ClusterRescheduledFlexiblePoints", rescheduledFlexiblePoints, 1, None)
    val rescheduledFlexiblePointsToChart = clusterRescheduledFlexiblePoints.centroid.toArray.toList.zipWithIndex.map(t => (t._2, t._1).asInstanceOf[(Number, Number)])


    LineChartSample.start(
      data = List(("fixedPoints", fixedPointsToChart),
        ("felxiblePoints", flexiblePointsToChart),
        ("rescheduledFlexiblePoints", rescheduledFlexiblePointsToChart)))
  }

  def approximateMethod(): Unit = {

    //println(findClosest(3.5, toListOfElements(List(1.0, 2.0, 3.0, 4.0, 5.0))))

    val input = List(7.0, 5.0)

    val output = sortByApproximation(input, List(-12.0, -17.0))

    println(input)
    println(output)

  }

  def findClosest(originalPoint: Double, listOfPoints: List[Element[Double]]): List[(Double, Element[Double])] =
    listOfPoints map { p => (norm(DenseVector(originalPoint) - DenseVector(p.value), 2), p) } sortBy(_._1)

  def sortDenseMatrixByApproximation(toApproximate: DenseMatrix[Double], fixed: DenseVector[Double]): List[Element[Double]] = {
    val row1 = toApproximate(0, ::).inner
    println(row1)
    val res = sortByApproximation(row1.data.toList, fixed.toArray.toList)
    println(res)
    res
  }

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
