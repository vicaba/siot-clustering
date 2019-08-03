package types.clusterer

trait PointLike extends Type {

  def assignedToCluster: Option[ClusterLike]

}
