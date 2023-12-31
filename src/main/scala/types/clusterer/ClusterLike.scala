package types.clusterer

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.clusterer.DataTypeMetadata.SyntheticDataType

trait ClusterLike extends Type {

  type ContainedElement

  def points: scala.collection.Set[ContainedElement]

  override def toString: String = s"Cluster($id, $userWiseSize)"

}

object ClusterLike {

  implicit def clusterToVector(c: ClusterLike): SyntheticDataType = c.syntheticValue

  implicit val toVector: DenseVectorReprOps[ClusterLike] = new DenseVectorReprOps[ClusterLike] {

    override def apply(t: ClusterLike): DenseVector[Double] = clusterToVector(t)

    override def zero(t: ClusterLike): DenseVector[Double] = t.dataTypeMetadata.EmptySyntheticData()
  }

}