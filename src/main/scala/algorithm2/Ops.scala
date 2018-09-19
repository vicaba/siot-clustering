package algorithm2

import breeze.linalg._
import types.{Point, Types2}
import types.Point._

object Ops {

  def findMirror(origin: Point, center: Point): Vector[Double] = {
    val diff = center.syntheticValue - origin.syntheticValue
    center.syntheticValue + diff
  }

  def findClosestMirror(origin: Point,
                        center: Point,
                        points: Seq[Point]): Seq[(Double, Point)] = {

    val idealMirror = findMirror(origin, center)

    points.zipWithIndex.map { case (point, idx) =>
      (norm(idealMirror - point.syntheticValue, 2), point)
    }.sortBy(_._1)
  }

  def main(args: Array[String]): Unit = {

    val points = List(
      DenseMatrix((5.0, 7.0)),
      DenseMatrix((7.0, 10.0)),
      DenseMatrix((9.0, 13.0))
    ).zipWithIndex.map {
      case (m, idx) =>
        Point(idx, m, None)(Types2)
    }.toVector

    assert(points(2).syntheticValue == findMirror(points(0), points(1)))

    assert(points(2).syntheticValue == findClosestMirror(points(0), points(1), points).head._2.syntheticValue)
  }

}
