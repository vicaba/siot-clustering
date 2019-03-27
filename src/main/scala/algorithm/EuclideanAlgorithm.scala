package algorithm
import algorithm.clusterer.EuclideanClusterer
import algorithm.scheduler.ClusterRescheduler
import com.typesafe.scalalogging.Logger
import types.Type
import types.mutable.Cluster

object EuclideanAlgorithm {

  type ClustererSettingsT = EuclideanClusterer.Settings

  type ReschedulerSettingsT = algorithm.scheduler.ReschedulerSettings

  val logger = Logger("Algorithm")

  case class ClustererOutput(settings: ClustererSettingsT, clusters: List[Cluster])

  case class ReschedulerOutput(settings: ReschedulerSettingsT, clusters: List[Cluster])

  case class ClustererAndReschedulerOutput(clustererOutput: ClustererOutput, reschedulerOutput: ReschedulerOutput)

  def clusterer(settings: ClustererSettingsT): List[Cluster] = EuclideanClusterer.apply(settings)

  def rescheduler(clusters: List[Cluster],
                  settings: ReschedulerSettingsT): List[(Cluster, List[algorithm.scheduler.PointChanged])] =
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

    logger.info(s"Running Clusterer. NumberOfClusters: {}. Points: {}",
                clustererSettings.numberOfClusters,
                clustererSettings.points.size)
    val clustererOutput = apply(clustererSettings)

    logger.info("Running Rescheduler.")

    val clustersCopy = Type.deepCopy(clustererOutput.clusters).asInstanceOf[List[Cluster]]

    val reschedulerResult = rescheduler(clustersCopy, reschedulerSettings)

    logger.info("End")

    ClustererAndReschedulerOutput(
      clustererOutput = clustererOutput,
      reschedulerOutput = ReschedulerOutput(reschedulerSettings, reschedulerResult.map(_._1))
    )

  }

}
