package algorithm

import algorithm.clusterer.{EuclideanClusterer, EuclideanClustererSettings}
import algorithm.scheduler.{ClusterRescheduler, ReschedulerSettings, UserDissatisfactionCalculator}
import com.typesafe.scalalogging.Logger
import config.GlobalConfig
import scheduler_model.ClusterAndAccumulatedLoadTransformer
import scheduler_model.load.AccumulatedLoad
import types.clusterer.Type
import types.clusterer.mutable.Cluster

object EuclideanAlgorithm {

  type ClustererSettingsT = EuclideanClustererSettings

  type ReschedulerSettingsT = ReschedulerSettings

  val logger = Logger("Algorithm")

  case class ClustererOutput(settings: ClustererSettingsT, clusters: List[Cluster])

  case class ReschedulerOutput(settings: ReschedulerSettingsT, clusters: List[Cluster], userDissatisfaction: Int)

  case class ClustererAndReschedulerOutput(clustererOutput: ClustererOutput, reschedulerOutput: ReschedulerOutput)

  private def clusterer(settings: ClustererSettingsT): List[Cluster] = EuclideanClusterer.apply(settings)

  private def rescheduler(clustersAsAccumulatedLoad: List[AccumulatedLoad],
                          settings: ReschedulerSettingsT): List[AccumulatedLoad] =
    ClusterRescheduler.apply(clustersAsAccumulatedLoad, settings)

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

    val unscheduledClustersAsAccumulatedLoad =
      ClusterAndAccumulatedLoadTransformer.apply(clustersCopy, clustersCopy.head.dataTypeMetadata).toList

    val scheduledClustersAsAccumulatedLoad =
      ClusterAndAccumulatedLoadTransformer.apply(clustersCopy, clustersCopy.head.dataTypeMetadata).toList

    scheduledClustersAsAccumulatedLoad.foreach(
      AccumulatedLoad.Mutate
        .splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(_, GlobalConfig.instance.sequenceSplitStrategy))

    val reschedulerResult = rescheduler(scheduledClustersAsAccumulatedLoad, reschedulerSettings)

    logger.info("End")

    ClustererAndReschedulerOutput(
      clustererOutput = clustererOutput,
      reschedulerOutput = ReschedulerOutput(
        reschedulerSettings,
        ClusterAndAccumulatedLoadTransformer
          .reverse(reschedulerResult, clustererOutput.clusters.head.dataTypeMetadata)
          .toList,
        UserDissatisfactionCalculator.listOfAccumulatedLoadsDissatisfaction(
          unscheduledClustersAsAccumulatedLoad,
          scheduledClustersAsAccumulatedLoad)
      )
    )

  }

}
