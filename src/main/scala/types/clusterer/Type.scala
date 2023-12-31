package types.clusterer

import types.clusterer
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster
import breeze.linalg._
import types.clusterer.DataTypeMetadata._

import scala.annotation.tailrec

object Type {

  def centroidOf[T <: Type](points: Seq[T]): DataTypeMetadata.SyntheticDataType = {
    val flattened = points.flatMap(_.flatten()).map(_.centroid).toList
    val sum = sumVectors(flattened, points.head.dataTypeMetadata.EmptySyntheticData())
    sum / flattened.length.toDouble
  }

  def flatten(points: TraversableOnce[Type]): Set[Point] = points.flatMap { p =>
    p.flatten()
  }.toSet

  /**
    * Converts a cluster or a point into a cluster. If _type is a [[types.clusterer.mutable.Cluster]], _type is returned. If _type is a [[types.clusterer.immutable.Point]],
    * a [[types.clusterer.mutable.Cluster]] that contains the _type is returned
    *
    * @param _type The type to convert from
    * @return the Cluster
    */
  def toCluster(_type: Type): types.clusterer.mutable.Cluster = _type match {
    case c: types.clusterer.mutable.Cluster => c
    case p: Point           => mutable.Cluster(p.id, p.id.toString, Set(p), 0, None)(p.dataTypeMetadata)
  }

  /**
    * Performs a deepCopy of _type, linked with reverse links with parent are preserved
    *
    * @param _type the object to be copied
    * @param parent the parent of the object
    * @return the deepCopy of _type
    */
  private def deepCopy(_type: Type, parent: Option[types.clusterer.mutable.Cluster]): Type = _type match {
    case c: types.clusterer.mutable.Cluster =>
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

  /**
    * Performs a deepCopy of _type. Note that references to upper elements in the hierarchy of _type will remain the
    * same (they are not copied)
    *
    * @param _type the object to be copied
    * @return the deepCopy of _type
    */
  def deepCopy(_type: Type): Type = {

    _type match {
      case c: types.clusterer.mutable.Cluster => deepCopy(c)
      case p: Point           => deepCopy(p)
    }

  }

  /**
    * Performs a deepCopy of c. Note that references to upper elements in the hierarchy of c will remain the
    * same (they are not copied)
    *
    * @param c the cluster to be copied
    * @return the deepCopy of c
    */
  def deepCopy(c: Cluster): Cluster = {
    val clusterCopy = c.deepCopy()
    clusterCopy.points.foreach(t => deepCopy(t, Some(clusterCopy)))
    clusterCopy
  }

  /**
    * Performs a deepCopy of p. Note that references to upper elements in the hierarchy of _type will remain the
    * same (they are not copied).
    *
    * @param p the point to be copied
    * @return the deepCopy of p
    */
  def deepCopy(p: Point): Point = {
    p.deepCopy()
  }

  /**
    * Calculates the sum of the given [[clusterer.DataTypeMetadata.DataType]] (sum of matrixes)
    *
    * @param remaining the remaining data to sum
    * @param accum the accumulated sum
    * @return the sum
    */
  @tailrec
  final def sumPoints(remaining: List[DataType], accum: DataType): DataType = remaining match {
    case e :: tail => sumPoints(tail, accum + e)
    case Nil       => accum
  }

  /**
    * Calculates the sum of the given [[clusterer.DataTypeMetadata.SyntheticDataType]] (sum of matrixes)
    *
    * @param remaining the remaining data to sum
    * @param accum the accumulated sum
    * @return the sum
    */
  @tailrec
  final def sumVectors(remaining: List[SyntheticDataType], accum: SyntheticDataType): SyntheticDataType =
    remaining match {
      case e :: tail => sumVectors(tail, accum + e)
      case Nil       => accum
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

  def userWiseSize: Int

  def flatten(): Set[Point]

  /**
    * See [[Type#sumPoints(scala.collection.immutable.List, breeze.linalg.DenseMatrix)]].
    */
  final def sumPoints(remaining: List[DataType], accum: DataType): DataType =
    Type.sumPoints(remaining, accum)

  /**
    * See [[Type#sumVectors(scala.collection.immutable.List, breeze.linalg.DenseVector)]].
    */
  final def sumVectors(remaining: List[SyntheticDataType], accum: SyntheticDataType): SyntheticDataType =
    Type.sumVectors(remaining, accum)

  override def toString: String = s"Type($id, $size)"

  def deepCopy(): ThisType

}
