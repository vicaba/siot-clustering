package algorithm

import algorithm.clusterer.{EuclideanClusterer, EuclideanClustererSettings}
import algorithm.scheduler.{ClusterRescheduler, ReschedulerSettings}
import com.typesafe.scalalogging.Logger
import scheduler_model.ClusterAndAccumulatedLoadTransformer
import scheduler_model.load.AccumulatedLoad
import types.clusterer.Type
import types.clusterer.mutable.Cluster

object EuclideanAlgorithm {

  type ClustererSettingsT = EuclideanClustererSettings

  type ReschedulerSettingsT = ReschedulerSettings

  val logger = Logger("Algorithm")

  case class ClustererOutput(settings: ClustererSettingsT, clusters: List[Cluster])

  case class ReschedulerOutput(settings: ReschedulerSettingsT, clusters: List[Cluster])

  case class ClustererAndReschedulerOutput(clustererOutput: ClustererOutput, reschedulerOutput: ReschedulerOutput)

  private def clusterer(settings: ClustererSettingsT): List[Cluster] = EuclideanClusterer.apply(settings)

  private def rescheduler(clusters: List[Cluster], settings: ReschedulerSettingsT): List[AccumulatedLoad] =
    ClusterRescheduler.apply(clusters, settings)

  def apply(clustererSettings: ClustererSettingsT): ClustererOutput = {

    logger.info(s"Running Clusterer. NumberOfClusters: {}. Points: {}",
                clustererSettings.numberOfClusters,
                clustererSettings.points.size)

    val clustererResult = clusterer(clustererSettings)

    ClustererOutput(clustererSettings, clustererResult)

  }

  def apply(clustererSettings: ClustererSettingsT,
            reschedulerSettings: ReschedulerSettingsT): ClustererAndReschedulerOutput = {

    val clustererOutput = apply(clustererSettings)

    logger.info("Running Rescheduler.")

    val clustersCopy = Type.deepCopy(clustererOutput.clusters).asInstanceOf[List[Cluster]]

    val reschedulerResult = rescheduler(clustersCopy, reschedulerSettings)

    logger.info("End")

    ClustererAndReschedulerOutput(
      clustererOutput = clustererOutput,
      reschedulerOutput = ReschedulerOutput(
        reschedulerSettings,
        ClusterAndAccumulatedLoadTransformer.reverse(reschedulerResult, clustererOutput.clusters.head.dataTypeMetadata).toList)
    )

  }

}
