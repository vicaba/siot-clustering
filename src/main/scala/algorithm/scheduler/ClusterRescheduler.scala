package algorithm.scheduler

import metrics.Metric
import com.typesafe.scalalogging.Logger
import test.EuclideanClustererToSchedulerDataTypeTransformation
import test.load.AccumulatedLoad
import types.clusterer.mutable.Cluster

object ClusterRescheduler {

  val logger = Logger("Rescheduler")

  def apply(clusters: List[Cluster], settings: ReschedulerSettings): List[AccumulatedLoad] = {

    val clustersAsAccumulatedLoad = clusters.zipWithIndex.map { case (cluster, idx) =>
      EuclideanClustererToSchedulerDataTypeTransformation.apply(idx, Cluster.flatten(cluster))
    }

    Scheduler.apply(clustersAsAccumulatedLoad,
                    settings.metricTransformation,
                    settings.userOrderings,
                    settings.schedulerAlgorithmOrderings)
  }

  def apply(cluster: Cluster, settings: ReschedulerSettings): AccumulatedLoad = apply(List(cluster), settings).head


}
