package algorithm.algorithms.euclidean

import algorithm.algorithms.GenAlgorithm
import algorithm.clusterer.EuclideanClusterer
import algorithm.scheduler.ClusterRescheduler
import types.mutable.Cluster

object EuclideanAlgorithm extends GenAlgorithm {

  override type ClustererSettingsT = EuclideanClusterer.Settings

  override type ReschedulerSettingsT = algorithm.scheduler.Settings

  override type BatchRunSettingsBuilderT = BatchRunSettingsBuilder

  override def clusterer(settings: ClustererSettingsT): List[Cluster] = EuclideanClusterer.apply(settings)

  override def rescheduler(clusters: List[Cluster],
                           settings: ReschedulerSettingsT): List[(Cluster, List[algorithm.scheduler.PointChanged])] =
    ClusterRescheduler.apply(clusters, settings)

}
