package types
import types.Types.{DataType, SyntheticDataType}
import types.immutable.Point
import types.mutable.Cluster

import scala.annotation.tailrec

object Type {

  def toCluster(_type: Type): mutable.Cluster = _type match {
    case c: mutable.Cluster => c
    case p: PointLike => mutable.Cluster(p.id, p.id.toString, Set(p), 0, None)(p.dataTypeMetadata)
  }

  private def deepCopy(_type: Type, parent: Option[mutable.Cluster]): Type = _type match {
    case c: mutable.Cluster =>
      if (parent.isDefined) {
        c.topLevel = parent
        c.points.foreach(deepCopy(_, Some(c)))
      } else {
        c.points.foreach(deepCopy(_, Some(c)))
      }
      c
    case p: Point => parent.get += p.setCluster(parent.get)
  }

  def deepCopy(types: Traversable[Type]): Traversable[Type] = types.map(deepCopy)

  def deepCopy(_type: Type): Type = {

    _type match {
      case c: mutable.Cluster => deepCopy(c)
      case p: Point => deepCopy(p)
    }

  }

  def deepCopy(c: Cluster): Cluster = {
    val clusterCopy = c.deepCopy()
    clusterCopy.points.foreach(t => deepCopy(t, Some(clusterCopy)))
    clusterCopy
  }

  def deepCopy(p: Point): Point = {
    p.deepCopy()
  }

}

trait Type {

  type ThisType <: Type

  def id: Int

  def data: DataType

  def syntheticValue: SyntheticDataType

  def centroid: SyntheticDataType

  def dataTypeMetadata: DataTypeMetadata

  def size: Int

  @tailrec
  final def sumPoints(remaining: List[DataType], accum: DataType): DataType = remaining match {
    case e :: tail => sumPoints(tail, accum + e)
    case Nil       => accum
  }

  @tailrec
  final def sumVectors(remaining: List[SyntheticDataType], accum: SyntheticDataType): SyntheticDataType =
    remaining match {
      case e :: tail => sumVectors(tail, accum + e)
      case Nil       => accum
    }

  override def toString: String = s"Type($id, $data)"

  def deepCopy(): ThisType

}
