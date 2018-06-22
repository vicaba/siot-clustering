package clustering

import clustering.util.Improvement
import types._
import metrics.Metric
import types.{Cluster, Point}

import scala.annotation.tailrec
import scala.util.Random

object Algorithm {

  def distanceTo(cluster: Cluster): Double =
    Metric.maxMin(cluster.syntheticCenter)

  /*  def apply(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric, iterations: Int): List[Cluster] = {

  }*/

  def run(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric, improvement: Double): List[Cluster] = {

    val _clusters = randomSample(numberOfClusters, points).zipWithIndex.map { case (point, idx) =>
      idx -> Cluster(idx, idx.toString, Set(point.setCluster(idx)))
    }.toMap

    val initialMetric = _clusters.values.foldLeft(0.0) { case (accum, cluster) => accum + metric(cluster) }

    @tailrec
    def assignToClusters(clusters: Map[Int, Cluster], remainingPoints: scala.Vector[Point], currentImprovement: Improvement): List[Cluster] =
      remainingPoints match {
        case p +: tail =>

          val bestClusterToAssign = clusters.values.minBy { cluster =>
            if (p.isAssignedToCluster) distanceTo(cluster - p) else distanceTo(cluster)
          }

          p.assignedToCluster.map(clusters(_))

          // TODO: Remove point from old cluster
          // TODO: When adding the point to the cluster, set the new clusterId to the point
          // TODO: What happens if the Map already has the key?

          assignToClusters(clusters + (bestClusterToAssign.id -> (bestClusterToAssign + p)), tail, currentImprovement)
        case IndexedSeq() =>
          val currentMetric = clusters.values.foldLeft(0.0) { case (accum, cluster) => accum + metric(cluster) }
          val improvedEnough = currentImprovement(currentMetric) < improvement
          // TODO: add memory
          if (!improvedEnough) assignToClusters(clusters, points, currentImprovement) else clusters.values.toList
      }

    assignToClusters(_clusters, points, new Improvement(initialMetric))

  }

  private def randomSample(take: Int, points: Seq[Point]): List[Point] = {
    val r = new Random(100)
    r.shuffle(points).take(take).toList
  }

}


