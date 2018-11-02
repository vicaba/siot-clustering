package algorithm.algorithms.brute
import algorithm.algorithms.GenAlgorithm
import algorithm.clusterer.BruteClusterer
import algorithm.scheduler.ClusterRescheduler
import types.Cluster

object BruteAlgorithm extends GenAlgorithm {

  override type ClustererSettingsT   = BruteClusterer.Settings

  override type ReschedulerSettingsT = ClusterRescheduler.Settings

  override type BatchRunSettingsBuilderT = algorithm.algorithms.brute.BatchRunSettingsBuilder

  override def clusterer(settings: ClustererSettingsT): List[Cluster] = BruteClusterer.apply(settings)

  override def rescheduler(clusters: List[Cluster],
    settings: ReschedulerSettingsT): List[(Cluster, List[ClusterRescheduler.PointChanged])] =
    ClusterRescheduler.apply(clusters, settings)

}
