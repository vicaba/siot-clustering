package cluster.scheduler

import cluster.Types.{Cluster, Point}
import cluster.scheduler.Rescheduler.MatrixResult
import metrics.Metric

import scala.annotation.tailrec

object ClusterRescheduler {

  private[ClusterRescheduler] class Memory[T](val list: Vector[T], val max: Int) {

    def areAllElementsEqual(): Boolean = !list.exists(_ != list.head)

    def +:(e: T): Memory[T] = {
      val newList = if (list.size > max) e +: list.dropRight(1) else e +: list

      new Memory[T](newList, max)
    }
  }

  object Memory {
    def apply[T](max: Int): Memory[T] = new Memory[T](Vector[T](), max)
  }

  private[ClusterRescheduler] class PointChange(val cluster: Cluster, val point: Point, val change: MatrixResult[Double])

  class PointChanged(val point: Point, val change: MatrixResult[Double])


  def rescheduleCluster(cluster: Cluster, improvement: Double, metric: Metric): (Cluster, List[PointChanged]) = {

    @tailrec
    def reschedule(improvementAchieved: Double, memory: Memory[Double], cluster: Cluster, changes: List[PointChanged]):
    (Cluster, List[PointChanged]) = {
      if (improvementAchieved <= improvement || memory.areAllElementsEqual()) (cluster, changes)
      else {
        val pointChange = rescheduleOnePoint(cluster, metric)
        val pointChanged = new PointChanged(pointChange.point, pointChange.change)
        val improvement = metric(cluster.syntheticCenter, pointChange.cluster.syntheticCenter)
        reschedule(improvement, improvement +: memory, pointChange.cluster, pointChanged +: changes)
      }
    }

    reschedule(improvement, Memory.apply(3), cluster, List.empty)

  }

  def rescheduleOnePoint(cluster: Cluster, metric: Metric): PointChange = {
    val pointToReschedule = cluster.points.maxBy { point =>
      metric(point.syntheticValue, cluster.syntheticCenter)
    }
    val rescheduleResult = Rescheduler.reschedule(pointToReschedule.values, cluster.syntheticCenter, metric)
    val rescheduledPoint = pointToReschedule.copy(values = rescheduleResult.matrix)
    val rescheduledCluster = cluster + rescheduledPoint

    new PointChange(rescheduledCluster, rescheduledPoint, rescheduleResult)
  }

}
