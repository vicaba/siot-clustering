package algorithm.clusterer.elementlocatorheuristic

import types.clusterer.DataTypeMetadata.SyntheticDataType
import types.clusterer.mutable.Cluster

/**
  * Heuristic decorator that prunes some elements before returning the results of applying a heuristic
  *
  * @param heuristic the heuristic
  */
case class HeuristicDecorator(heuristic: ElementLocatorHeuristic) extends ElementLocatorHeuristic {
  override def apply(v1: Cluster, v2: SyntheticDataType, v3: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] = {
    val clusters              = heuristic.apply(v1, v2, v3)
    val aprioriElementsToDrop = clusters.length / 2
    val elementsToDrop        = if (aprioriElementsToDrop < 1) aprioriElementsToDrop + 1 else aprioriElementsToDrop
    clusters.dropRight(aprioriElementsToDrop)
  }
}
