package types


trait PointLike extends Type {

  def assignedToCluster: Option[ClusterLike]

}
