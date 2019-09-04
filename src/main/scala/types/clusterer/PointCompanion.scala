package types.clusterer

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.clusterer.DataTypeMetadata._
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster
import types.clusterer.mutable.Cluster.toVector

trait PointCompanion {

  import scala.language.implicitConversions

  def toCluster(point: Point): Cluster = Type.toCluster(point)

  implicit def pointListToVector(list: List[Point]): Option[SyntheticDataType] = list.map(_.syntheticValue) match {
    case Nil   => None
    case _list => Some(_list.tail.foldLeft(_list.head) { case (accum, vector) => accum + vector })
  }

  def flatten(ps: TraversableOnce[Point]): Set[Point] = ps.toSet

  def flatten(p: Point): Set[Point] = Set(p)

  implicit def pointToVector(p: Point): SyntheticDataType = toVector.apply(p)

  implicit val toVector: DenseVectorReprOps[Point] = new DenseVectorReprOps[Point] {

    override def apply(t: Point): DenseVector[Double] = t.syntheticValue

    override def zero(t: Point): DenseVector[Double] = t.dataTypeMetadata.EmptySyntheticData()
  }

  implicit def toVectorTraversable[S[X] <: Traversable[X]]: DenseVectorReprOps[S[Point]] = new DenseVectorReprOps[S[Point]] {

    override def apply(t: S[Point]): DenseVector[Double] = {
      val syntheticValues = t.toList.map(_.syntheticValue)
      Type.sumVectors(syntheticValues, t.head.dataTypeMetadata.EmptySyntheticData())
    }

    override def zero(t: S[Point]): DenseVector[Double] = toVector.zero(t.head)

  }



}
