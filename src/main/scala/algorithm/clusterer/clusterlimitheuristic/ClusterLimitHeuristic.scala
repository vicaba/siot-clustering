package algorithm.clusterer.clusterlimitheuristic

import types.mutable.Cluster

trait ClusterLimitHeuristic extends ((Cluster, Cluster) => Boolean) {
  def apply(clusterBefore: Cluster, clusterAfter: Cluster): Boolean
}
