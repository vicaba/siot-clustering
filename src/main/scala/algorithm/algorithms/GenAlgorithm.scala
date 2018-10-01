package algorithm.algorithms

import algorithm.scheduler.ClusterRescheduler.PointChanged
import com.typesafe.scalalogging.Logger
import types.Cluster

trait GenAlgorithm {

  type ClustererSettings <: Settings

  type ReschedulerSettings <: Settings

  case class StepT[Settings](settings: Settings, clusters: List[Cluster])

  case class Steps(_1: StepT[ClustererSettings], _2: StepT[ReschedulerSettings])

  val logger = Logger("Algorithm")

  def clusterer(settings: ClustererSettings): List[Cluster]

  def rescheduler(clusters: List[Cluster], settings: ReschedulerSettings): List[(Cluster, List[PointChanged])]

  def apply(clustererSettings: ClustererSettings, reschedulerSettings: ReschedulerSettings): Steps = {
    logger.info("Clusterer")

    val clustererResult = clusterer(clustererSettings)

    logger.info("Rescheduler")

    val reschedulerResult = rescheduler(clustererResult, reschedulerSettings)

    logger.info("End")

    Steps(
      _1 = StepT(clustererSettings, clustererResult),
      _2 = StepT(reschedulerSettings, reschedulerResult.map(_._1))
    )
  }

}
