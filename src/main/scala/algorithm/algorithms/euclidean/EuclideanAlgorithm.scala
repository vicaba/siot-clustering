package algorithm.algorithms.euclidean
import algorithm.algorithms.GenAlgorithm
import algorithm.clusterer.EuclideanClusterer
import algorithm.scheduler.ClusterRescheduler
import types.Cluster

object EuclideanAlgorithm extends GenAlgorithm {

  override type ClustererSettings = EuclideanClusterer.Settings

  override type ReschedulerSettings = ClusterRescheduler.Settings

  override def clusterer(settings: ClustererSettings): List[Cluster] = EuclideanClusterer.apply(settings)

  override def rescheduler(clusters: List[Cluster],
                           settings: ReschedulerSettings): List[(Cluster, List[ClusterRescheduler.PointChanged])] =
    ClusterRescheduler.apply(clusters, settings)

}
