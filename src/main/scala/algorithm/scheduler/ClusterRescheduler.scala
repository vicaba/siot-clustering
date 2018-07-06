package algorithm.scheduler

import types._
import algorithm.scheduler.Rescheduler.MatrixResult
import metrics.Metric
import collection._
import types.{Cluster, Point}

import scala.annotation.tailrec

object ClusterRescheduler {

  case class Settings(metric: Metric, improvement: Double, memory: Int = 2)

  class PointChange(val cluster: Cluster, val point: Point, val change: MatrixResult[Double])

  class PointChanged(val point: Point, val change: MatrixResult[Double])

  def apply(cluster: Cluster, settings: Settings): (Cluster, List[PointChanged]) =
    rescheduleCluster(cluster, settings.metric, settings.improvement, settings.memory)

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
        if (pointChange.isDefined) {
          val pointChanged = new PointChanged(pointChange.get.point, pointChange.get.change)
          val _currentMetric = metric(pointChange.get.cluster)
          reschedule(_currentMetric, _currentMetric +: memory, pointChange.get.cluster, pointChanged +: changes)
        } else {
          reschedule(currentClusterMetric, 0 +: memory, cluster, changes)
        }
      }
    }

    reschedule(initialMetric, Memory(cluster.points.size), cluster, List.empty)

  }

  def rescheduleOnePoint(cluster: Cluster, metric: Metric): Option[PointChange] = {
    val pointToReschedule = cluster.points.maxBy { point =>
      metric(cluster) - metric(cluster - point)
    }

    val rescheduleResult = Rescheduler.reschedule(pointToReschedule.values, cluster - pointToReschedule, metric)

    rescheduleResult.map { result =>
      val rescheduledPoint = pointToReschedule.copy(values = result.matrix)
      val rescheduledCluster = cluster + rescheduledPoint

      new PointChange(rescheduledCluster, rescheduledPoint, result)
    }
  }

}
