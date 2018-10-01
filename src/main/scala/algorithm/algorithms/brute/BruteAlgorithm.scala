package algorithm.algorithms.brute
import algorithm.algorithms.GenAlgorithm
import algorithm.clusterer.BruteClusterer
import algorithm.scheduler.ClusterRescheduler
import types.Cluster

object BruteAlgorithm extends GenAlgorithm {

  override type ClustererSettings   = BruteClusterer.Settings

  override type ReschedulerSettings = ClusterRescheduler.Settings

  override def clusterer(settings: ClustererSettings): List[Cluster] = BruteClusterer.apply(settings)

  override def rescheduler(clusters: List[Cluster],
    settings: ReschedulerSettings): List[(Cluster, List[ClusterRescheduler.PointChanged])] =
    ClusterRescheduler.apply(clusters, settings)

}
