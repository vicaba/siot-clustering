package types

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.Types.{DataType, SyntheticDataType}

case class Point(id: Int, values: DataType, assignedToCluster: Option[Int] = None)(implicit val types: TypesT) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: Point => this.id == p.id
    case _        => false
  }

  override def hashCode(): Int = this.id

  override def toString: String = s"Point($id, $syntheticValue, $assignedToCluster)"

  def setCluster(clusterId: Int): Point = this.copy(assignedToCluster = Some(clusterId))

  def isAssignedToCluster: Boolean = this.assignedToCluster.isDefined

  def setValues(values: DataType): Point = this.copy(values = values)

  def syntheticValue: SyntheticDataType = types.synthesizeValues(this.values)

}

object Point {

  import scala.language.implicitConversions

  implicit def pointListToVector(list: List[Point]): Option[SyntheticDataType] = list.map(_.syntheticValue) match {
    case Nil   => None
    case _list => Some(_list.tail.foldLeft(_list.head) { case (accum, vector) => accum + vector })
  }

  implicit def pointToVector(p: Point): SyntheticDataType = toVector.apply(p)

  implicit val toVector: DenseVectorReprOps[Point] = new DenseVectorReprOps[Point] {

    override def apply(t: Point): DenseVector[Double] = t.syntheticValue

    override def zero(t: Point): DenseVector[Double] = t.types.EmptySyntheticData()
  }

}
