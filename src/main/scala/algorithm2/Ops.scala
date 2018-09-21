package algorithm2

import java.io.PrintWriter

import breeze.linalg.{max, _}
import config.Configuration
import play.api.libs.json.Json
import types.{Cluster, Point, Types2}
import types.Point._
import types.Types.SyntheticDataType
import types.serialization.TypesJsonSerializer._

import scala.math._
import scala.util.Random

object Ops {

  /**
    * Given two points A and B, finds C as the mirror image of point A reflected into the plane perpendicular to line AB
    * and situated at distance norm(AB, 2) from A
    *
    * @param a the point to find the mirror image of
    * @param c the point where a perpendicular plane to line AB passes through
    * @return the mirror image
    */
  def findMirror(a: SyntheticDataType, c: SyntheticDataType): SyntheticDataType = (2.0 * c) - a

  /**
    * Given a set of points, finds the closest point to the mirror image of point A. See [[findClosestMirror()]].
    *
    * @param origin the point to find the mirror image of
    * @param center the point where a perpendicular plane to line AB passes through, acting as the mirror
    * @param points the set of points to find the closest to the image of point A
    * @return the closest mirror image
    */
  def findClosestMirror(origin: Point, center: SyntheticDataType, points: IndexedSeq[Point]): IndexedSeq[(Double, Point)] = {

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
  def findClosestMirror(origin: SyntheticDataType,
                        center: SyntheticDataType,
                        points: IndexedSeq[SyntheticDataType]): IndexedSeq[(Double, SyntheticDataType)] = {

    val idealMirror = findMirror(origin, center)

    points.zipWithIndex
      .map {
        case (point, idx) =>
          (norm(idealMirror - point, 2), point)
      }
      .sortBy(_._1)
  }

  def centroid(points: IndexedSeq[Point]): types.Types.SyntheticDataType =
    points.foldLeft(points.head.types.EmptySyntheticData()) {
      case (accum, p) =>
        accum + p.syntheticValue
    } / points.length.toDouble


  def createClusters(clusters: List[Cluster], freePoints: scala.Vector[Point]): Unit = {

    val centroid = if (clusters.isEmpty) centroid(freePoints) else clusters.foldLeft(clusters.head.types.EmptySyntheticData()) {
      case (accum, c) => accum + c.centroid
    }

    freePoints match {
      case p +: tail =>
        val remainingPoints = tail - p
        val mirror = findClosestMirror(p, centroid, remainingPoints)
        val lastCreatedCluster = clusters.head.id
        val cluster = Cluster(lastCreatedCluster + 1, s"${lastCreatedCluster + 1}", p + mirror)(p.types)
        createClusters(cluster :: clusters, remainingPoints)
      case IndexedSeq() =>
    }

  }

    points match {
    case p :+ tail =>
      findClosestMirror()
    case IndexedSeq() =>
  }

  def generateRandom2DPoints(center: Vector[Double], radius: Double, numberOfPoints: Int, angle: Double): IndexedSeq[Vector[Double]] = {

    for (_ <- 0 to numberOfPoints) yield {
      // Random from [0, 1]
      val random = () => Random.nextDouble()
      val angle = 2 * Pi * random()
      val r = radius * sqrt(random())
      val x = r * cos(angle) + center(0)
      val y = r * sin(angle) + center(1)
      Vector[Double](x, y)
    }

  }

  def main(args: Array[String]): Unit = {

    val genPoints = generateRandom2DPoints(Vector(0.0, 0.0), 5, 100, 5).zipWithIndex.map {
      case (m, idx) =>
        Point(idx, m.toDenseVector.asDenseMatrix, None)(Types2)
    }.toVector

    Some(new PrintWriter("files/output/genPoints.json")).foreach { p =>
      p.write(Json.prettyPrint(Json.toJson(genPoints.toList.map(_.syntheticValue))).toString())
      p.close()
    }


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
