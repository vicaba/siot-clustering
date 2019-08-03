package algorithm.clusterer.keepclusteringheuristic

import types.clusterer.mutable.Cluster

trait KeepClusteringHeuristic extends ((Cluster, Cluster) => Boolean) {
  def apply(c: Cluster, elementToAdd: Cluster): Boolean
}
