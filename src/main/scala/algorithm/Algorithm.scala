package algorithm

import algorithm.clusterer.Clusterer
import algorithm.scheduler.ClusterRescheduler
import com.typesafe.scalalogging.Logger
import types.Cluster

object Algorithm {

  val logger = Logger("algorithm")

  case class Step(id: Int, clusters: List[Cluster])

  trait StepT {
    val clusters: List[Cluster]
    val aggregatedMetric: Double
  }

  case class Step1(settings: Clusterer.Settings,
                   override val clusters: List[Cluster],
                   override val aggregatedMetric: Double)
      extends StepT

  case class Step2(
      settings: ClusterRescheduler.Settings,
      override val clusters: List[Cluster],
      override val aggregatedMetric: Double
  ) extends StepT

  case class Steps(_1: Step1, _2: Step2)

  def apply(clustererSettings: Clusterer.Settings, reschedulerSettings: ClusterRescheduler.Settings): Steps = {

    logger.info("clusterer")

    val clustererResult = Clusterer(clustererSettings)

    logger.info("rescheduler")

    val reschedulerResult = clustererResult.map(ClusterRescheduler(_, reschedulerSettings)._1).toList

    logger.info("end")

    Steps(
      _1 = Step1(clustererSettings, clustererResult, clustererSettings.metric.aggregateOf(clustererResult)),
      _2 = Step2(reschedulerSettings, reschedulerResult, reschedulerSettings.metric.aggregateOf(reschedulerResult))
    )

  }

}
