package algorithm

import metrics.Metric
import types._

import scala.annotation.tailrec
import scala.util.Random

object Clusterer {

  case class Settings(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric, times: Int = 1)

  def apply(settings: Settings): List[Cluster] = {

    def aggregateErrorOf(clusters: List[Cluster]): Double =
      clusters.foldLeft(0.0) { case (accum, cluster) => accum + settings.metric(cluster) }

    (for (i <- 0 until settings.times)
      yield runOnce(settings.numberOfClusters, settings.points, settings.metric)).minBy(cl => aggregateErrorOf(cl))
  }

  def distanceTo(cluster: Cluster, averagePointsPerCluster: Int): Double =
    0.7*Metric.par(cluster) + 0.3*cluster.points.size/averagePointsPerCluster

  private def runOnce(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric): List[Cluster] = {

    val averagePointsPerCluster = points.length / numberOfClusters

    val randomSamplePoints = randomSample(numberOfClusters, points)
    val r = new Random(System.currentTimeMillis)
    val shuffledPointsWithoutClusterSeeds = r.shuffle((points.toSet -- randomSamplePoints.toSet).toVector)

    val _clusters = randomSamplePoints.zipWithIndex.map { case (point, idx) =>
      idx -> Cluster(idx, idx.toString, Set(point.setCluster(idx)))
    }.toMap

    val initialMetric = metric(Cluster.Empty ++ points)

    implicit def clusterToTuple(c: Cluster): (Int, Cluster) = c.id -> c

    def collectPoints(clusters: Traversable[Cluster]): scala.Vector[Point] =
      clusters.foldLeft(clusters.head.points) {
        case (accum, cluster) => accum ++ cluster.points
      }.toVector

    @tailrec
    def assignToClusters(clusters: Map[Int, Cluster], remainingPoints: scala.Vector[Point]): List[Cluster] =
      remainingPoints match {
        case p +: tail =>

          val bestClusterToAssign = clusters.values.minBy { cluster =>
            if (p.isAssignedToCluster) distanceTo(cluster, averagePointsPerCluster) else distanceTo(cluster + p, averagePointsPerCluster)
          }

          if (p.assignedToCluster.isDefined) {

            val pointAssignedToCluster = p.assignedToCluster.get

            if (pointAssignedToCluster == bestClusterToAssign.id) assignToClusters(clusters, tail)
            else assignToClusters(
              clusters ++ Map(clusters(p.assignedToCluster.get) - p, bestClusterToAssign + p)
              , tail)
          } else assignToClusters(
            clusters + (bestClusterToAssign + p)
            , tail)
        case IndexedSeq() =>
          val currentMetric = clusters.values.foldLeft(0.0) { case (accum, cluster) => accum + metric(cluster) } / clusters.size
          clusters.values.toList
      }

    // First round without the points assigned to each cluster
    assignToClusters(_clusters, shuffledPointsWithoutClusterSeeds)

  }

  private def randomSample(take: Int, points: Seq[Point]): List[Point] = {
    val r = new Random(System.currentTimeMillis)
    r.shuffle(points).take(take).toList
  }

}
