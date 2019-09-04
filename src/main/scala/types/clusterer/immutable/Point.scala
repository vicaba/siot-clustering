package types.clusterer.immutable

import types.clusterer.DataTypeMetadata.{DataType, SyntheticDataType}
import types.clusterer.mutable.Cluster
import types.clusterer.{DataTypeMetadata, Type, PointCompanion}

case class Point(
    override val id: Int,
    override val data: DataType,
    dataLabels: List[String] = Nil,
  assignedToCluster: Option[Cluster] = None)(implicit override val dataTypeMetadata: DataTypeMetadata)
    extends Type {

  override type ThisType = Point

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: Point => this.id == p.id
    case _        => false
  }

  /**
    * Returns the number of rows of data in this Point
    * @return
    */
  override def size: Int = data.rows

  override def hashCode(): Int = this.id

  override def toString: String = s"Point($id, $syntheticValue, $assignedToCluster)"

  def setCluster(clusterId: Cluster): Point = this.copy(assignedToCluster = Some(clusterId))

  def isAssignedToCluster: Boolean = this.assignedToCluster.isDefined

  def setValues(data: DataType, dataLabels: List[String] = Nil): Point = this.copy(data = data)

  def syntheticValue: SyntheticDataType = dataTypeMetadata.synthesizeValues(this.data)

  def toCluster: Cluster = Point.toCluster(this)

  override def centroid: SyntheticDataType = syntheticValue / data.rows.toDouble

  override def deepCopy(): ThisType = this.copy()

  override def flatten(): Set[Point] = Set(this)

}

object Point extends PointCompanion