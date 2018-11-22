package types

import types.Types.Cluster

trait PointLike extends Type {

  def assignedToCluster: Option[Cluster]

}
