package algorithm.clusterer.clusterlimitheuristic

import types.mutable.Cluster

trait ClusterLimitHeuristic {
  def apply(clusterBefore: Cluster, clusterAfter: Cluster): Boolean
}
