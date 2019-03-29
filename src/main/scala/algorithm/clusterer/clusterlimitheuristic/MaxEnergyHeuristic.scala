package algorithm.clusterer.clusterlimitheuristic

import breeze.linalg.sum
import types.mutable.Cluster

object MaxEnergyHeuristic extends ClusterLimitHeuristic {
  override def apply(clusterBefore: Cluster, clusterAfter: Cluster): Boolean = {
    val maxEnergyAllowed   = sum(clusterBefore.centroid) * (clusterBefore.points.size + 1)
    val potentialMaxEnergy = sum(clusterAfter.centroid) * clusterAfter.points.size
    potentialMaxEnergy <= maxEnergyAllowed
  }
}
