package algorithm.scheduler

import metrics.Metric
import com.typesafe.scalalogging.Logger
import types.mutable.Cluster

object ClusterRescheduler {

  val logger = Logger("Rescheduler")

  def apply(clusters: List[Cluster], settings: ReschedulerSettings): List[(Cluster, List[Nothing])] = Nil

  def apply(cluster: Cluster, settings: ReschedulerSettings): (Cluster, List[Nothing]) = (null, null)

  // TODO: Make sure that the change is mutable
  def rescheduleOnePoint(cluster: Cluster, metric: Metric): Option[Nothing] = None

}
