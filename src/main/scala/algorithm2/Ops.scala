package algorithm2

import java.io.PrintWriter

import akka.actor.ProviderSelection
import breeze.linalg.{max, _}
import config.Configuration
import eventmanager.{EventManager, Subscriber}
import main.Main.readEgaugeData
import play.api.libs.json.{Json, Writes}
import types.{Cluster, Point, Types2}
import types.Point._
import types.Types.SyntheticDataType
import types.serialization.TypesJsonSerializer._
import types.serialization.ClusterJsonSerializer._
import util.FileUtils

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq
import scala.math._
import scala.util.Random

object Ops {

  type Heuristic = (Cluster, SyntheticDataType, IndexedSeq[Cluster]) => Option[Cluster]

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
  def findClosestMirrors(origin: Cluster,
                         center: SyntheticDataType,
                         points: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] = {

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
  def findClosestMirrors(origin: SyntheticDataType,
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

  def findClosestMirror(origin: SyntheticDataType,
                        center: SyntheticDataType,
                        points: IndexedSeq[SyntheticDataType]): Option[SyntheticDataType] =
    findClosestMirrors(origin, center, points).headOption.map(_._2)

  def findClosestMirror(origin: Cluster, center: SyntheticDataType, points: IndexedSeq[Cluster]): Option[Cluster] =
    findClosestMirrors(origin, center, points).headOption.map(_._2)

  def centroidOf(points: Seq[Point]): types.Types.SyntheticDataType =
    points.foldLeft(points.head.types.EmptySyntheticData()) {
      case (accum, p) =>
        accum + p.syntheticValue
    } / points.length.toDouble

  @tailrec
  def clustersToClusters(centroid: SyntheticDataType,
                         freeClusters: IndexedSeq[Cluster],
                         heuristic: Heuristic,
                         clusters: LinearSeq[Cluster] = LinearSeq()): LinearSeq[Cluster] = {
    freeClusters match {
      case c +: tail =>
        val closestMirror = heuristic(c, centroid, tail)
        if (closestMirror.isEmpty) c +: clusters
        else {
          val mirrorIndex =
            tail.indexWhere(_.centroid == closestMirror.get.centroid)
          val mirror             = tail(mirrorIndex)
          val remainingClusters  = tail.patch(mirrorIndex, IndexedSeq(), 1)
          val lastCreatedCluster = clusters.headOption.map(_.id).getOrElse(1)
          val cluster =
            Cluster(lastCreatedCluster + 1, s"${lastCreatedCluster + 1}", c.points ++ mirror.points)(c.types)
          clustersToClusters(centroid, remainingClusters, heuristic, cluster +: clusters)
        }
      case IndexedSeq() => clusters
    }
  }

  def cluster(stopAtKClusters: Int,
              stopAtIterationCount: Int,
              clusters: LinearSeq[Cluster],
              heuristic: Heuristic = findClosestMirror): LinearSeq[Cluster] = {

    val points   = clusters.flatMap(_.points)
    val centroid = centroidOf(points)

    var iterations              = 0
    var kClusters               = clusters.size
    var _clusters: Seq[Cluster] = clusters

    if (clusters.isEmpty) return Nil
    if (stopAtKClusters == 1) return List(Cluster(1, "1", points.toSet)(clusters.head.types))

    EventManager.singleton.publish("clusters", _clusters.toList)

    while (iterations < stopAtIterationCount && kClusters > stopAtKClusters) {

      _clusters = clustersToClusters(centroid, _clusters.toVector, heuristic, Nil)
      iterations = iterations + 1
      kClusters = _clusters.size

      EventManager.singleton.publish("clusters", _clusters.toList)

    }
    _clusters.toList

  }

  def generateRandom2DPoints(center: Vector[Double],
                             radius: Double,
                             numberOfPoints: Int,
                             angle: Double): IndexedSeq[Vector[Double]] = {

    for (_ <- 1 to numberOfPoints) yield {
      // Random from [0, 1]
      val angle = 2 * Pi * Random.nextDouble()
      val r     = radius * sqrt(Random.nextDouble())
      val x     = r * cos(angle) + center(0)
      val y     = r * sin(angle) + center(1)
      Vector[Double](x, y)
    }

  }

  def main(args: Array[String]): Unit = {

    val genPoints = generateRandom2DPoints(Vector(0.0, 0.0), 5, 50, 5).zipWithIndex.map {
      case (m, idx) =>
        Cluster(idx, idx.toString, Set(Point(idx, m.toDenseVector.asDenseMatrix, None)(Types2)))(Types2)
    }.toList

    var clustersBuffer: List[List[Cluster]] = Nil
    EventManager.singleton
      .subscribe("clusters",
                 (topic: String, event: Object) => clustersBuffer = event.asInstanceOf[List[Cluster]] :: clustersBuffer)

    /*    readEgaugeData("files/input/egauge.json").map { p =>
      Cluster(p.id, p.id.toString, Set(p))(p.types)
    }.toList.take(10)*/

    val clusters = cluster(4, Int.MaxValue, genPoints)
    println(clusters.flatMap(_.points).size)

    Some(new PrintWriter("files/output/cluster.json")).foreach { p =>
      val json = clustersBuffer.zipWithIndex.map {
        case (clusteringIteration, idx) =>
          Json.obj(
            "iteration" -> idx,
            "clusters"  -> Json.toJson(clusteringIteration.toList)
          )
      }
      p.write(Json.prettyPrint(Json.toJson(json)).toString)
      p.close()
    }

    FileUtils.copyFile("files/output/cluster.json", "/Users/vicaba/Projects/jupyter/shared/siot-eclustering-viz/files")

  }

}
