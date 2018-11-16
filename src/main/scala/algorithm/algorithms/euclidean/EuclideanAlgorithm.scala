package algorithm.algorithms.euclidean
import algorithm.algorithms.GenAlgorithm
import algorithm.clusterer.EuclideanClusterer
import algorithm.clusterer.FlattenedEuclideanClusterer.Settings
import algorithm.clusterer.FlattenedEuclideanClusterer
import algorithm.scheduler.{ClusterRescheduler, ClusterRescheduler2}
import types.Cluster

object EuclideanAlgorithm extends GenAlgorithm {

  override type ClustererSettingsT = Settings

  override type ReschedulerSettingsT = ClusterRescheduler.Settings

  override type BatchRunSettingsBuilderT = BatchRunSettingsBuilder

  override def clusterer(settings: ClustererSettingsT): List[Cluster] = EuclideanClusterer.apply(settings)

  override def rescheduler(clusters: List[Cluster],
                           settings: ReschedulerSettingsT): List[(Cluster, List[ClusterRescheduler.PointChanged])] =
    ClusterRescheduler2.apply(clusters, settings)

}
