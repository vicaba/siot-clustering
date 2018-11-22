package types.immutable

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.Types.{DataType, SyntheticDataType}
import types.mutable.Cluster
import types.{DataTypeMetadata, PointLike, Type}

case class Point(override val id: Int, override val data: DataType, override val assignedToCluster: Option[Cluster] = None)(
    implicit override val dataTypeMetadata: DataTypeMetadata)
    extends PointLike {

  override type ThisType = Point

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: Point => this.id == p.id
    case _        => false
  }

  override def size: Int = 1

  override def hashCode(): Int = this.id

  override def toString: String = s"Point($id, $syntheticValue, $assignedToCluster)"

  def setCluster(clusterId: Cluster): Point = this.copy(assignedToCluster = Some(clusterId))

  def isAssignedToCluster: Boolean = this.assignedToCluster.isDefined

  def setValues(data: DataType): Point = this.copy(data = data)

  def syntheticValue: SyntheticDataType = dataTypeMetadata.synthesizeValues(this.data)

  def toCluster: Cluster = Point.toCluster(this)

  override def centroid: SyntheticDataType = syntheticValue / dataTypeMetadata.Rows.toDouble

  override def deepCopy(): ThisType = this.copy()

}

object Point {

  import scala.language.implicitConversions

  def toCluster(point: Point): Cluster = Type.toCluster(point)

  implicit def pointListToVector(list: List[Point]): Option[SyntheticDataType] = list.map(_.syntheticValue) match {
    case Nil   => None
    case _list => Some(_list.tail.foldLeft(_list.head) { case (accum, vector) => accum + vector })
  }

  implicit def pointToVector(p: Point): SyntheticDataType = toVector.apply(p)

  implicit val toVector: DenseVectorReprOps[Point] = new DenseVectorReprOps[Point] {

    override def apply(t: Point): DenseVector[Double] = t.syntheticValue

    override def zero(t: Point): DenseVector[Double] = t.dataTypeMetadata.EmptySyntheticData()
  }

}
