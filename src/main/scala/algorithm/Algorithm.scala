package algorithm

import algorithm.scheduler.ClusterRescheduler
import types.{Cluster, Point}

import scala.collection.mutable.ListBuffer

object Algorithm {

  case class Step(id: Int, clusters: List[Cluster])

  def apply(
    preClusteringSettings: Clusterer.Settings
    , reschedulerSettings: ClusterRescheduler.Settings): List[Step] = {
    val steps = ListBuffer[Step]()

    val preClusterResult = Clusterer(preClusteringSettings)

    steps.+=(Step(1, preClusterResult))

    val rescheduleResult = preClusterResult.map(ClusterRescheduler(_, reschedulerSettings)._1)

    steps.+=(Step(2, rescheduleResult))

    steps.toList

  }

}
