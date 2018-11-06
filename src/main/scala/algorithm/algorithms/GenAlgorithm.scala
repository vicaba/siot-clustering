package algorithm.algorithms

import algorithm.scheduler.ClusterRescheduler.PointChanged
import com.typesafe.scalalogging.Logger
import types._

trait GenAlgorithm {

  type ClustererSettingsT <: Settings

  type ReschedulerSettingsT <: Settings

  type BatchRunSettingsBuilderT <: BatchRunSettingsBuilder[this.type]

  case class StepT[Settings](settings: Settings, clusters: List[Cluster])

  case class Steps(_1: StepT[ClustererSettingsT], _2: StepT[ReschedulerSettingsT])

  val logger = Logger("Algorithm")

  def clusterer(settings: ClustererSettingsT): List[Cluster]

  def rescheduler(clusters: List[Cluster], settings: ReschedulerSettingsT): List[(Cluster, List[PointChanged])]

  def apply(clustererSettings: ClustererSettingsT): StepT[ClustererSettingsT] = {

    logger.info("Clusterer. Settings")

    val clustererResult = clusterer(clustererSettings)

    StepT(clustererSettings, clustererResult)

  }

  def apply(clustererSettings: ClustererSettingsT, reschedulerSettings: ReschedulerSettingsT): Steps = {

    logger.info("Clusterer")

    val step1 = apply(clustererSettings)

    logger.info("Rescheduler")

    val reschedulerResult = rescheduler(step1.clusters, reschedulerSettings)

    logger.info("End")

    Steps(
      _1 = step1,
      _2 = StepT(reschedulerSettings, reschedulerResult.map(_._1))
    )
  }

}
