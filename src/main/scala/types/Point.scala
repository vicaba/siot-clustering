package types

import types.Types.{DataType, SyntheticDataType}

case class Point(id: Int, values: DataType, assignedToCluster: Option[Int] = None)(implicit val types: TypesT) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: Point => this.id == p.id
    case _ => false
  }

  override def hashCode(): Int = this.id

  override def toString: String = s"Point($id, $syntheticValue, $assignedToCluster)"

  def setCluster(clusterId: Int): Point = this.copy(assignedToCluster = Some(clusterId))

  def isAssignedToCluster: Boolean = this.assignedToCluster.isDefined

  def setValues(values: DataType): Point = this.copy(values = values)

  def syntheticValue: SyntheticDataType = types.synthesizeValues(this.values)

}

