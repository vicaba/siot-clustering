package algorithm.scheduler

import types._
import algorithm.scheduler.Rescheduler.MatrixResult
import algorithm.util.RelativeImprovement
import metrics.Metric
import collection._
import types.immutable.Point
import types.mutable.Cluster
import types.ops.SetOps._

import scala.annotation.tailrec

object ClusterRescheduler {

  def apply(clusters: List[Cluster], settings: Settings): List[(Cluster, List[PointChanged])] = {

    // TODO: make tail recursive. Use trampoline?
    def retrieveLeafClusters(clusters: List[Type]): List[Cluster] = {
      clusters.flatMap {
        case point: Point =>
          // By forcing hierarchyLevel == 0 we are forcing bottom and leaf points only
          point.assignedToCluster.toList.flatMap { c => if (c.hierarchyLevel == 0) c.topLevel else Nil
          }
        case cluster: Cluster => retrieveLeafClusters(cluster.points.toList)
      }
    }

    val leafClusters: List[Cluster] = retrieveLeafClusters(clusters).distinct

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

    @tailrec
    def reschedule(currentClusterMetric: Double,
                   relativeImprovement: RelativeImprovement[Cluster],
                   cluster: Cluster,
                   changes: List[PointChanged]): (Cluster, List[PointChanged]) = {

      println(relativeImprovement.average)

      if (relativeImprovement.hasImprovedEnough || relativeImprovement.isStuck)
        (relativeImprovement.getBest._2, changes)
      else {
        val pointChange = rescheduleOnePoint(cluster, metric)
        if (pointChange.isDefined) {
          val pointChanged   = new PointChanged(pointChange.get.point, pointChange.get.change)
          val _currentMetric = metric(pointChange.get.cluster)
          //TODO: Cluster.deepCopy()?
          reschedule(_currentMetric, relativeImprovement.feed(_currentMetric, cluster), pointChange.get.cluster, pointChanged +: changes)
        } else {
          (relativeImprovement.getBest._2, changes)
        }
      }
    }

    // TODO: Replace oldCluster with the rescheduledOne
    val newCluster = reschedule(initialMetric, RelativeImprovement((initialMetric, Type.deepCopy(cluster)), 0.01, cluster.size + (cluster.size / 2)), cluster, List.empty)

    (cluster.setPoints(newCluster._1.points), Nil)

  }

  // TODO: Make sure that the change is mutable
  def rescheduleOnePoint(cluster: Cluster, metric: Metric): Option[PointChange] = {

    val pointToReschedule =
      cluster.points
        .asInstanceOf[Set[Cluster]]
        .flatMap { c =>
          c.points.foreach { p =>
            if (p.isInstanceOf[Cluster])
              println("Cluster")
          }
          c.points
        }
        .asInstanceOf[Set[Point]]
        .maxBy { point =>

        val m1 = metric(cluster)
          val m2 = metric(cluster - point.toCluster)

          metric(cluster) - metric(cluster - point.toCluster)
        }

    val rescheduleResult = Rescheduler.reschedule(pointToReschedule.data, cluster - pointToReschedule, metric)

    rescheduleResult.map { result =>
      cluster += pointToReschedule.setValues(result.matrix).toCluster

      new PointChange(cluster, pointToReschedule, result)
    }

  }

}
