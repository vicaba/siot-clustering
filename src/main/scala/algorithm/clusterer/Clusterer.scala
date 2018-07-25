package algorithm.clusterer

import metrics.Metric
import types._

import scala.annotation.tailrec
import scala.util.Random

object Clusterer {

  case class Settings(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric, times: Int = 1)

  def apply(settings: Settings): List[Cluster] = {

    def aggregateErrorOf(clusters: List[Cluster]): Double = settings.metric.aggregateOf(clusters)

    (for (i <- 0 until settings.times)
      yield {
        runOnce(settings.numberOfClusters, settings.points, settings.metric)
      }).minBy(cl => aggregateErrorOf(cl))
  }

  def distanceTo(cluster: Cluster, averagePointsPerCluster: Int): Double =
    Metric.par(cluster)

  private def runOnce(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric): List[Cluster] = {

    implicit def clusterToTuple(c: Cluster): (Int, Cluster) = c.id -> c

    def collectPoints(clusters: Traversable[Cluster]): scala.Vector[Point] =
      clusters.foldLeft(clusters.head.points) {
        case (accum, cluster) => accum ++ cluster.points
      }.toVector

    @tailrec
    def assignToClusters(clusters: Map[Int, Cluster], remainingPoints: scala.Vector[Point], distanceF: Cluster => Double, metrics: List[Double]): List[Cluster] =
      remainingPoints match {
        case p +: tail =>

          val bestClusterToAssign = clusters.values.minBy { cluster =>
            p.assignedToCluster.map { cId =>
              if (cId == cluster.id) distanceF(cluster - p) else  distanceF(cluster + p)
            }.getOrElse(distanceF(cluster + p))
          }

          if (p.assignedToCluster.isDefined) {

            val pointAssignedToCluster = p.assignedToCluster.get

            if (pointAssignedToCluster == bestClusterToAssign.id) assignToClusters(clusters, tail, distanceF, metrics)
            else assignToClusters(
              clusters ++ Map(clusters(p.assignedToCluster.get) - p, bestClusterToAssign + p)
              , tail, distanceF, metrics)
          } else assignToClusters(
            clusters + (bestClusterToAssign + p)
            , tail, distanceF, metrics)
        case IndexedSeq() =>
          val currentMetric = metric.aggregateOf(clusters.values.toList)
          if (metrics.contains(currentMetric)) clusters.values.toList
          else assignToClusters(clusters, collectPoints(clusters.values), distanceF, metrics :+ currentMetric)
      }

    if (points.nonEmpty) {

      implicit val types: TypesT = points.head.types

      val averagePointsPerCluster = points.length / numberOfClusters

      val randomSamplePoints = randomSample(numberOfClusters, points)
      val r = new Random(System.currentTimeMillis)
      val shuffledPointsWithoutClusterSeeds = r.shuffle((points.toSet -- randomSamplePoints.toSet).toVector)

      val _clusters = randomSamplePoints.zipWithIndex.map { case (point, idx) =>
        idx -> Cluster(idx, idx.toString, Set(point.setCluster(idx)))
      }.toMap

      // First round without the points assigned to each cluster
      assignToClusters(_clusters, shuffledPointsWithoutClusterSeeds, distanceTo(_, averagePointsPerCluster), Nil)

    } else Nil

  }

  private def randomSample(take: Int, points: Seq[Point]): List[Point] = {
    val r = new Random(System.currentTimeMillis)
    r.shuffle(points).take(take).toList
  }

}
