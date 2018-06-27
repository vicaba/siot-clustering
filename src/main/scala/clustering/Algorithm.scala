package clustering

import clustering.util.Improvement
import collection.Memory
import types._
import metrics.Metric
import types.{Cluster, Point}

import scala.annotation.tailrec
import scala.util.Random

object Algorithm {

  case class Run(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric, improvement: Double) {
    def apply(): List[Cluster] = run(numberOfClusters, points, metric, improvement)
  }

  def runIterative(run: Run, times: Int): List[Cluster] = {

    def aggregateErrorOf(clusters: List[Cluster]): Double =
      clusters.foldLeft(0.0) { case (accum, cluster) => accum + run.metric(cluster) }

    val highestError = run.metric.Highest * run.numberOfClusters
    (for (i <- 0 until times) yield run()).minBy(cl => aggregateErrorOf(cl))
  }

  def distanceTo(cluster: Cluster): Double =
    Metric.par(cluster.syntheticCenter)

  def run(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric, improvement: Double): List[Cluster] = {

    val randomSamplePoints = randomSample(numberOfClusters, points)

    val _clusters = randomSamplePoints.zipWithIndex.map { case (point, idx) =>
      idx -> Cluster(idx, idx.toString, Set(point.setCluster(idx)))
    }.toMap

    val initialMetric = _clusters.values.foldLeft(0.0) { case (accum, cluster) => accum + metric(cluster) }

    implicit def clusterToTuple(c: Cluster): (Int, Cluster) = c.id -> c

    def collectPoints(clusters: Traversable[Cluster]): scala.Vector[Point] =
      clusters.foldLeft(clusters.head.points) {
      case (accum, cluster) => accum ++ cluster.points
    }.toVector

    @tailrec
    def assignToClusters(clusters: Map[Int, Cluster], remainingPoints: scala.Vector[Point], memory: Memory[Double], currentImprovement: Improvement): List[Cluster] =
      remainingPoints match {
        case p +: tail =>

          val bestClusterToAssign = clusters.values.minBy { cluster =>
            if (p.isAssignedToCluster) distanceTo(cluster - p + p) else distanceTo(cluster + p)
          }

          if (p.assignedToCluster.isDefined) {

            val pointAssignedToCluster = p.assignedToCluster.get

            if (pointAssignedToCluster == bestClusterToAssign.id) assignToClusters(clusters, tail, memory, currentImprovement)
            else assignToClusters(
              clusters ++ Map(clusters(p.assignedToCluster.get) - p, bestClusterToAssign + p)
              , tail
              , memory
              , currentImprovement)
          } else assignToClusters(
            clusters + (bestClusterToAssign + p)
            , tail
            , memory
            , currentImprovement)
        case IndexedSeq() =>
          val currentMetric = clusters.values.foldLeft(0.0) { case (accum, cluster) => accum + metric(cluster) }
          val improvedEnough = improvement < currentImprovement(currentMetric) || memory.areAllElementsEqual()
          // TODO: add memory
          if (!improvedEnough) assignToClusters(clusters, collectPoints(clusters.values), currentMetric +: memory, currentImprovement) else clusters.values.toList
      }

    // First round without the points assigned to each cluster
    assignToClusters(_clusters, (points.toSet -- randomSamplePoints.toSet).toVector, Memory(3), new Improvement(initialMetric))

  }

  private def randomSample(take: Int, points: Seq[Point]): List[Point] = {
    val r = new Random(System.currentTimeMillis)
    r.shuffle(points).take(take).toList
  }

}


