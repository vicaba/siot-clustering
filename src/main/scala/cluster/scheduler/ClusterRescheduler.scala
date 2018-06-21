package cluster.scheduler

import cluster.Types.{Cluster, Point}
import cluster.scheduler.Rescheduler.MatrixResult
import metrics.Metric
import collection._

import scala.annotation.tailrec

object ClusterRescheduler {

  private[ClusterRescheduler] class PointChange(val cluster: Cluster, val point: Point, val change: MatrixResult[Double])

  class PointChanged(val point: Point, val change: MatrixResult[Double])

  /**
   * Reschedules the points inside the cluster in order to minimize the metric function.
   *
   * @param cluster The cluster to reschedule
   * @param metric The metric function
   * @param improvement Fraction of unity. Stop when this improvement is reached
   * @param memory When the cluster metric doesn't change after memory reschedules, stop.
   * @return The rescheduled cluster.
   */
  def rescheduleCluster(cluster: Cluster, metric: Metric, improvement: Double, memory: Int = 2):
  (Cluster, List[PointChanged]) = {

    val initialMetric = metric(cluster)

    def improvementPercentage(improvement: Double): Double = 1 - improvement / initialMetric

    @tailrec
    def reschedule(currentClusterMetric: Double, memory: Memory[Double], cluster: Cluster, changes: List[PointChanged]):
    (Cluster, List[PointChanged]) = {

      val improvedEnough = improvementPercentage(currentClusterMetric) > improvement

      if (improvedEnough || memory.areAllElementsEqual()) (cluster, changes)
      else {
        val pointChange = rescheduleOnePoint(cluster, metric)
        val pointChanged = new PointChanged(pointChange.point, pointChange.change)
        val _currentMetric = metric(pointChange.cluster)
        reschedule(_currentMetric, _currentMetric +: memory, pointChange.cluster, pointChanged +: changes)
      }
    }

    reschedule(0, Memory(memory), cluster, List.empty)

  }

  def rescheduleOnePoint(cluster: Cluster, metric: Metric): PointChange = {
    val pointToReschedule = cluster.points.maxBy { point =>
      metric(cluster) - metric(cluster - point)
    }
    val rescheduleResult = Rescheduler.reschedule(pointToReschedule.values, cluster.syntheticCenter, metric)
    val rescheduledPoint = pointToReschedule.copy(values = rescheduleResult.matrix)
    val rescheduledCluster = cluster + rescheduledPoint

    new PointChange(rescheduledCluster, rescheduledPoint, rescheduleResult)
  }

}
