package algorithm.algorithms.euclidean
import algorithm.algorithms.GenAlgorithm
import algorithm.clusterer._
import algorithm.scheduler.ClusterRescheduler
import types.mutable.Cluster

object MutableEuclideanAlgorithm extends GenAlgorithm {

  override type Cluster = types.mutable.Cluster

  override type ClustererSettings = EuclideanClusterer.Settings

  override type ReschedulerSettings = ClusterRescheduler.Settings

  override def clusterer(settings: ClustererSettings): List[Cluster] = mutable.EuclideanClusterer.apply(settings)

  override def rescheduler(clusters: List[Cluster],
                           settings: ReschedulerSettings): List[(Cluster, List[ClusterRescheduler.PointChanged])] = ???
    //ClusterRescheduler.apply(clusters, settings)

}
