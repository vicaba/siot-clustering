package types.immutable

import types.DataTypeMetadata.{DataType, SyntheticDataType}
import types.mutable.Cluster
import types.{DataTypeMetadata, PointLike, PointLikeCompanion, Type}

case class Point(
    override val id: Int,
    override val data: DataType,
    dataLabels: List[String] = Nil,
    override val assignedToCluster: Option[Cluster] = None)(implicit override val dataTypeMetadata: DataTypeMetadata)
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

  def setValues(data: DataType, dataLabels: List[String] = Nil): Point = this.copy(data = data)

  def syntheticValue: SyntheticDataType = dataTypeMetadata.synthesizeValues(this.data)

  def toCluster: Cluster = Point.toCluster(this)

  override def centroid: SyntheticDataType = syntheticValue / dataTypeMetadata.Rows.toDouble

  override def deepCopy(): ThisType = this.copy()

}

object Point extends PointLikeCompanion[Point, Cluster] {
  override def toCluster(point: Point): Cluster = Type.toCluster(point)
}
