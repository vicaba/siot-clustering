package algorithm.scheduler

import algorithm.scheduler.ClusterRescheduler.{PointChange, PointChanged, Settings}
import types._
import algorithm.scheduler.Rescheduler.MatrixResult
import metrics.Metric
import collection._
import types.{Cluster, Point}

import scala.annotation.tailrec

object ClusterRescheduler2 {

  def apply(clusters: List[Cluster], settings: Settings): List[(Cluster, List[PointChanged])] = {

    // TODO: make tail recursive. Use trampoline?
    def retrieveLeafClusters(clusters: List[Types.Type]): List[Cluster] = {
      clusters.flatMap {
        case cluster: Cluster if cluster.hierarchyLevel > 2  => retrieveLeafClusters(cluster.points.toList)
        case cluster: Cluster if cluster.hierarchyLevel == 2 => List(cluster)
        case _                                               => Nil
      }
    }

    val leafClusters: List[Cluster] = retrieveLeafClusters(clusters)

    println("hola")

    val newClusterConfiguration =
      if (leafClusters.nonEmpty)
        leafClusters.map(rescheduleCluster(_, settings.metric, settings.improvement, settings.memory))
      else clusters.map((_, Nil))
    newClusterConfiguration
  }

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
  def rescheduleCluster(cluster: Cluster,
                        metric: Metric,
                        improvement: Double,
                        memory: Int = 2): (Cluster, List[PointChanged]) = {

    val initialMetric = metric(cluster)

    def improvementPercentage(improvement: Double): Double = 1 - improvement / initialMetric

    @tailrec
    def reschedule(currentClusterMetric: Double,
                   memory: Memory[Double],
                   cluster: Cluster,
                   changes: List[PointChanged]): (Cluster, List[PointChanged]) = {

      val improvedEnough = improvementPercentage(currentClusterMetric) > improvement

      if (improvedEnough || memory.areAllElementsEqual())
        if (improvedEnough)
          (cluster, changes)
        else
          (cluster, changes)
      else {
        val pointChange = rescheduleOnePoint(cluster, metric)
        if (pointChange.isDefined) {
          val pointChanged   = new PointChanged(pointChange.get.point, pointChange.get.change)
          val _currentMetric = metric(pointChange.get.cluster)
          reschedule(_currentMetric, _currentMetric +: memory, pointChange.get.cluster, pointChanged +: changes)
        } else {
          reschedule(currentClusterMetric, 0 +: memory, cluster, changes)
        }
      }
    }

    reschedule(initialMetric, Memory(cluster.points.size), cluster, List.empty)

    (cluster, Nil)

  }

  // TODO: Make sure that the change is mutable
  def rescheduleOnePoint(cluster: Cluster, metric: Metric): Option[PointChange] = {

    println("hola")

    cluster.points
      .asInstanceOf[Set[Cluster]]
      .flatMap { c =>
        c.points.foreach { p => if (p.isInstanceOf[Cluster]) println("Cluster") else println("Point")
        }
        c.points
      }
      .asInstanceOf[Set[Cluster]]
      .flatMap { c => c.points
      }
      .asInstanceOf[Set[Point]]

    val pointToReschedule =
      cluster.points
        .asInstanceOf[Set[Cluster]]
        .flatMap(_.points)
        .asInstanceOf[Set[Cluster]]
        .flatMap(_.points)
        .asInstanceOf[Set[Point]]
        .maxBy { point => metric(cluster) - metric(cluster - point)
        }

    val rescheduleResult = Rescheduler.reschedule(pointToReschedule.data, cluster - pointToReschedule, metric)

    rescheduleResult.map { result =>
      val rescheduledPoint   = pointToReschedule.copy(data = result.matrix)(pointToReschedule.types)
      val rescheduledCluster = cluster += rescheduledPoint.toCluster // Look here

      new PointChange(rescheduledCluster, rescheduledPoint, result)
    }

  }

}
