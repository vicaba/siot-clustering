package types

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import metrics.DenseVectorReprOps
import spire.algebra.{InnerProductSpace, VectorSpace}
import types.Types.{DataType, SyntheticDataType}

import scala.annotation.tailrec

case class Cluster(override val id: Int, name: String, points: Set[Point])(implicit override val types: TypesT)
    extends Types.Cluster {

  override type ContainedElement = Point

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: Cluster => this.id == c.id
    case _          => false
  }

  override def hashCode(): Int = this.id

  def isEmpty: Boolean = points.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def +(point: Point): Cluster = this.copy(points = (this.points - point) + point.setCluster(id))

  def ++(points: Seq[Point]): Cluster = this.copy(points = (this.points -- points) ++ points.map(_.setCluster(id)))

  def setPoints(points: Seq[Point]): Cluster = this.copy(points = points.map(_.setCluster(id)).toSet)

  def -(point: Point): Cluster = this.copy(points = points - point)

  def centroid: SyntheticDataType =
    points.foldLeft(types.EmptySyntheticData()) {
      case (accum, p) =>
        accum + p.syntheticValue
    } / points.size.toDouble

  /**
    * Calling this method without any point in the cluster is unsafe
    *
    * @return
    */
  def syntheticValue: SyntheticDataType = {

    val syntheticValues = this.points.toList.map(_.syntheticValue)

    sumVectors(syntheticValues, types.EmptySyntheticData())
  }

  override def data: DataType = {

    val values = this.points.toList.map(_.data)

    sumPoints(values, types.EmptyData())

  }

}

object Cluster {

  import scala.language.implicitConversions

  def Empty(implicit types: TypesT): Cluster = Cluster(-1, "empty", Set.empty)(types)

  implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticValue

  implicit val toVector: DenseVectorReprOps[Cluster] = new DenseVectorReprOps[Cluster] {

    override def apply(t: Cluster): DenseVector[Double] = clusterToVector(t)

    override def zero(t: Cluster): DenseVector[Double] = t.types.EmptySyntheticData()
  }

}
