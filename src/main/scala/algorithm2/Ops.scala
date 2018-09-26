package algorithm2

import java.io.PrintWriter

import breeze.linalg.{max, _}
import eventmanager.{EventManager, Subscriber}
import main.Main.readEgaugeData
import play.api.libs.json.{Json, Writes}
import types.{Cluster, Point, Types, Types2}
import types.Point._
import types.Types.SyntheticDataType
import types.serialization.TypesJsonSerializer._
import types.serialization.ClusterJsonSerializer._
import util.FileUtils

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq

object Ops {

  type Heuristic = (Cluster, SyntheticDataType, IndexedSeq[Cluster]) => Option[Cluster]

  def centroidOf[T <: Types.Type](points: Seq[T]): types.Types.SyntheticDataType =
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
              heuristic: Heuristic = MirrorImage.findClosestMirror(_, _, _)(MirrorImage.MirroredCluster)): LinearSeq[Cluster] = {

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


  def main(args: Array[String]): Unit = {

    val genPoints = Generator.generateRandom2DPoints(Vector(0.0, 0.0), 5, 50, 5).zipWithIndex.map {
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
