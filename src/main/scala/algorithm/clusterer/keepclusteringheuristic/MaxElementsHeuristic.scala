package algorithm.clusterer.keepclusteringheuristic

import types.mutable.Cluster

class MaxElementsHeuristic(val maxElements: Integer) extends KeepClusteringHeuristic {

  override def apply(c: Cluster, elementToAdd: Cluster): Boolean = c.points.size + 1 <= maxElements

}
