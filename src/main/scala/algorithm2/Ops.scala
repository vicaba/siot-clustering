package algorithm2

import breeze.linalg._
import types.{Point, Types2}
import types.Point._

object Ops {

  /**
    * Given two points A and B, finds C as the mirror image of point A reflected into the plane perpendicular to line AB
    * and situated at distance norm(AB, 2) from A
    *
    * @param a the point to find the mirror image of
    * @param c the point where a perpendicular plane to line AB passes through
    * @return the mirror image
    */
  def findMirror(a: Vector[Double], c: Vector[Double]): Vector[Double] = (2.0 * c) - a

  /**
    * Given a set of points, finds the closest point to the mirror image of point A. See [[findClosestMirror()]].
    *
    * @param origin the point to find the mirror image of
    * @param center the point where a perpendicular plane to line AB passes through, acting as the mirror
    * @param points the set of points to find the closest to the image of point A
    * @return the closest mirror image
    */
  def findClosestMirror(origin: Point, center: Point, points: Seq[Point]): Seq[(Double, Point)] = {

    val idealMirror = findMirror(origin, center)

    points.zipWithIndex
      .map {
        case (point, idx) =>
          (norm(idealMirror - point.syntheticValue, 2), point)
      }
      .sortBy(_._1)
  }

  /**
    * Given a set of points, finds the closest point to the mirror image of point A. See [[findClosestMirror()]].
    *
    * @param origin the point to find the mirror image of
    * @param center the point where a perpendicular plane to line AB passes through, acting as the mirror
    * @param points the set of points to find the closest to the image of point A
    * @return the closest mirror image
    */
  def findClosestMirror(origin: Vector[Double], center: Vector[Double], points: Seq[Vector[Double]]): Seq[(Double, Vector[Double])] = {

    val idealMirror = findMirror(origin, center)

    points.zipWithIndex
      .map {
        case (point, idx) =>
          (norm(idealMirror - point, 2), point)
      }
      .sortBy(_._1)
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

    val pointsWithoutExactMirror = List(
      DenseMatrix((5.0, 7.0)),
      DenseMatrix((7.0, 10.0)),
      DenseMatrix((10.0, 13.0))
    ).zipWithIndex.map {
      case (m, idx) =>
        Point(idx, m, None)(Types2)
    }.toVector

    assert(Vector(9.0, 13.0) == findMirror(points(0), points(1)))

    val cm = findClosestMirror(points(0), points(1), pointsWithoutExactMirror)

    assert(pointsWithoutExactMirror(2).syntheticValue == findClosestMirror(
      points(0),
      points(1),
      pointsWithoutExactMirror).head._2.syntheticValue)

  }

}
