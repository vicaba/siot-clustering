package types

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.Types.SyntheticDataType

trait PointLikeCompanion[P <: PointLike, C <: ClusterLike] {

  import scala.language.implicitConversions

  def toCluster(point: P): C  // = Type.toCluster(point)

  implicit def pointListToVector(list: List[P]): Option[SyntheticDataType] = list.map(_.syntheticValue) match {
    case Nil   => None
    case _list => Some(_list.tail.foldLeft(_list.head) { case (accum, vector) => accum + vector })
  }

  implicit def pointToVector(p: P): SyntheticDataType = toVector.apply(p)

  implicit val toVector: DenseVectorReprOps[P] = new DenseVectorReprOps[P] {

    override def apply(t: P): DenseVector[Double] = t.syntheticValue

    override def zero(t: P): DenseVector[Double] = t.dataTypeMetadata.EmptySyntheticData()
  }

}