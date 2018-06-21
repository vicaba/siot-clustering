package types

import types.Types.{DataType, SyntheticDataType, synthesizeValues}

case class Point(id: Int, values: DataType, assignedToCluster: Option[Int] = None) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: Point => this.id == p.id
    case _ => false
  }

  def setCluster(clusterId: Int): Point = this.copy(assignedToCluster = Some(clusterId))

  def isAssignedToCluster: Boolean = this.assignedToCluster.isDefined

  def setValues(values: DataType): Point = this.copy(values = values)

  def syntheticValue: SyntheticDataType = synthesizeValues(this.values)

}

