package types

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import metrics.DenseVectorReprOps
import spire.algebra.{InnerProductSpace, VectorSpace}
import types.Types.{DataType, SyntheticDataType}
import types.ops.SetOps._

import scala.annotation.tailrec

case class Cluster private (override val id: Int,
                            name: String,
                            private val points: scala.collection.mutable.Set[Types.Type],
                            private var hierarchyLevel: Int = 0,
                            private var topLevel: Option[Cluster] = None)(implicit override val types: TypesT)
    extends Types.Cluster {

  override type ContainedElement = Types.Type

  def copy(id: Int = this.id,
           name: String = this.name,
           points: scala.collection.mutable.Set[Types.Type] = this.points,
           hierarchylevel: Int = this.hierarchyLevel,
           topLevel: Option[Cluster] = this.topLevel): Cluster = {
    new Cluster(id, name, points, hierarchyLevel, topLevel)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: Cluster => this.id == c.id
    case _          => false
  }

  override def hashCode(): Int = this.id

  def isEmpty: Boolean = points.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def setHierarchyLevel(_hierarchyLevel: Int): Cluster = {
    this.hierarchyLevel = _hierarchyLevel
    this
  }

  def setTopLevel(_topLevel: Option[Cluster]): Cluster = {
    this.topLevel = _topLevel
    this
  }

  def +=(point: Types.Type): Cluster = {
    this.points += typeTransform(point)
    this
  }

  def -=(point: Point): Cluster = {
    this.points -= typeTransform(point)
    this
  }

  def ++=(points: TraversableOnce[Types.Type]): Cluster = {
    this.points ++= points.map(typeTransform)
    this
  }

  def --=(points: TraversableOnce[Types.Type]): Cluster = {
    this.points --= points.map(typeTransform)
    this
  }

  def setPoints(points: TraversableOnce[Types.Type]): Cluster = {
    (this --= points) ++= points.map(typeTransform)
    this
  }

  def +(point: Types.Type): Cluster = {
    val newPoints = this.points.toSet -/+ typeTransform(point)
    this.copy(points = mutableSetOf(newPoints))
  }

  def -(point: Types.Type): Cluster = {
    val newPoints = this.points.toSet - typeTransform(point)
    this.copy(points = mutableSetOf(newPoints))
  }

  def ++(points: TraversableOnce[Cluster]): Cluster = {
    val newPoints = this.points.toSet --/++ points.map(typeTransform)
    this.copy(points = mutableSetOf(newPoints))
  }

  def --(points: TraversableOnce[Cluster]): Cluster = {
    val newPoints = this.points.toSet -- points.map(typeTransform)
    this.copy(points = mutableSetOf(newPoints))
  }

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

  private def typeTransform(_type: Types.Type): Types.Type = _type match {
    case p: Point   => p.setCluster(id)
    case c: Cluster => c.setTopLevel(Some(this)).setHierarchyLevel(this.hierarchyLevel - 1)
  }

  def mutableSetOf[A](s: TraversableOnce[A]): scala.collection.mutable.Set[A] =
    Cluster.mutableSetOf(s)

}

object Cluster {

  def mutableSetOf[A](s: TraversableOnce[A]): scala.collection.mutable.Set[A] =
    new scala.collection.mutable.HashSet[A]() ++= s

  import scala.language.implicitConversions

  def apply(id: Int, name: String, points: TraversableOnce[Types.Type], hierarchyLevel: Int, topLevel: Option[Cluster])(
      implicit types: TypesT): Cluster = {
    new Cluster(id, name, mutableSetOf(points), hierarchyLevel, topLevel)(types)
  }

  def Empty(implicit types: TypesT): Cluster = Cluster(-1, "empty", Set.empty)(types)

  implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticValue

  implicit val toVector: DenseVectorReprOps[Cluster] = new DenseVectorReprOps[Cluster] {

    override def apply(t: Cluster): DenseVector[Double] = clusterToVector(t)

    override def zero(t: Cluster): DenseVector[Double] = t.types.EmptySyntheticData()
  }

}
