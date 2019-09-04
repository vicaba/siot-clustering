package algorithm.clusterer.keepclusteringheuristic

import breeze.linalg.sum
import types.clusterer.Type
import types.clusterer.mutable.Cluster

object MaxEnergyHeuristic extends KeepClusteringHeuristic {

  override def apply(currentCluster: Cluster, elementToAdd: Cluster): Boolean = {

    val maxEnergyAllowed   = sum(currentCluster.centroid) * (currentCluster.points.size + 1)
    val potentialMaxEnergy = sum(Type.centroidOf(List(currentCluster, elementToAdd))) * (currentCluster.points.size + 1)
    val res                = potentialMaxEnergy <= maxEnergyAllowed
    res
  }

}
