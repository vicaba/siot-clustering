package algorithm.clusterer.elementlocatorheuristic

import types.DataTypeMetadata.SyntheticDataType
import types.mutable.Cluster

/**
  * Chains a list of heuristics
  *
  * @param heuristics the list of heuristics
  */
class HeuristicChain(heuristics: List[ElementLocatorHeuristic]) extends ElementLocatorHeuristic {
  override def apply(v1: Cluster, v2: SyntheticDataType, v3: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] =
    heuristics.tail.foldLeft(heuristics.head.apply(v1, v2, v3)) {
      case (_clusters, _heuristic) =>
        _heuristic(v1, v2, _clusters.map(_._2))
    }
}

object HeuristicChain {
  def apply(heuristic: ElementLocatorHeuristic): HeuristicChain = new HeuristicChain(List(heuristic))
}
