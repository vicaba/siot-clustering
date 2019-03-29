package algorithm.clusterer.elementlocatorheuristic

import types.DataTypeMetadata.SyntheticDataType
import types.mutable.Cluster

trait ElementLocatorHeuristic
  extends ((Cluster, SyntheticDataType, IndexedSeq[Cluster]) => IndexedSeq[(Double, Cluster)]) {
  override def apply(v1: Cluster, v2: SyntheticDataType, v3: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] = ???
}

object ElementLocatorHeuristic {
  def apply(f: (Cluster, SyntheticDataType, IndexedSeq[Cluster]) => IndexedSeq[(Double, Cluster)])
  : ElementLocatorHeuristic =
    new ElementLocatorHeuristic {
      override def apply(v1: Cluster, v2: SyntheticDataType, v3: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] =
        f(v1, v2, v3)
    }
}
