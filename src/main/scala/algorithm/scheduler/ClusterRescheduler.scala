package algorithm.scheduler

import metrics.Metric
import com.typesafe.scalalogging.Logger
import test.{ClusterAndAccumulatedLoadTransformer, SequenceSplitByConsecutiveElements}
import test.load.{AccumulatedLoad, Load}
import types.clusterer.mutable.Cluster

object ClusterRescheduler {

  val logger = Logger("Rescheduler")

  def apply(clusters: List[Cluster], settings: ReschedulerSettings): List[AccumulatedLoad] = {

    val clustersAsAccumulatedLoad = ClusterAndAccumulatedLoadTransformer.apply(clusters).toList

    clustersAsAccumulatedLoad.foreach(Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      _,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage))


    Scheduler.apply(clustersAsAccumulatedLoad,
                    settings.metricTransformation,
                    settings.userOrderings,
                    settings.schedulerAlgorithmOrderings)
  }

  def apply(cluster: Cluster, settings: ReschedulerSettings): AccumulatedLoad = apply(List(cluster), settings).head

}
