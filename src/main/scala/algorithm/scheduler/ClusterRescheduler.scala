package algorithm.scheduler

import metrics.Metric
import com.typesafe.scalalogging.Logger
import scheduler_model.ClusterAndAccumulatedLoadTransformer
import scheduler_model.load.{AccumulatedLoad, Load}
import scheduler_model.sequence_split.SequenceSplitByConsecutiveElements
import types.clusterer.mutable.Cluster

object ClusterRescheduler {

  val logger = Logger("Rescheduler")

  def apply(clusters: List[Cluster], settings: ReschedulerSettings): List[AccumulatedLoad] = {

    val clustersAsAccumulatedLoad = ClusterAndAccumulatedLoadTransformer.apply(clusters).toList

    clustersAsAccumulatedLoad.foreach(AccumulatedLoad.Mutate.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      _,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage))


    Scheduler.apply(clustersAsAccumulatedLoad,
                    settings.metricTransformation,
                    settings.userOrderings,
                    settings.schedulerAlgorithmOrderings)
  }

  def apply(cluster: Cluster, settings: ReschedulerSettings): AccumulatedLoad = apply(List(cluster), settings).head

}
