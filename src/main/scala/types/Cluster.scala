package types

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import metrics.DenseVectorReprOps
import spire.algebra.{InnerProductSpace, VectorSpace}
import types.Types.{DataType, SyntheticDataType}
import types.ops.SetOps._

import scala.annotation.tailrec
import scala.collection

case class Cluster private (override val id: Int,
                            name: String,
                            private val _points: scala.collection.mutable.Set[Types.Type],
                            private var hierarchyLevel: Int = 0,
                            private var topLevel: Option[Cluster] = None)(implicit override val types: TypesT)
    extends Types.Cluster {

  override type ContainedElement = Types.Type

  def copy(id: Int = this.id,
           name: String = this.name,
           points: TraversableOnce[Types.Type] = this._points,
           hierarchylevel: Int = this.hierarchyLevel,
           topLevel: Option[Cluster] = this.topLevel): Cluster = {
    new Cluster(id, name, mutableSetOf(points), hierarchyLevel, topLevel)(types)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: Cluster => this.id == c.id
    case _          => false
  }

  override def hashCode(): Int = this.id

  def isEmpty: Boolean = _points.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def setPoints(points: TraversableOnce[Types.Type]): Cluster = {
    (this --= points) ++= points.map(typeTransform)
    this
  }

  override def points: collection.Set[Types.Type] = _points.toSet

  def setHierarchyLevel(_hierarchyLevel: Int): Cluster = {
    this.hierarchyLevel = _hierarchyLevel
    this
  }

  def setTopLevel(_topLevel: Option[Cluster]): Cluster = {
    this.topLevel = _topLevel
    this
  }

  def +=(point: Types.Type): Cluster = {
    this._points += typeTransform(point)
    this
  }

  def -=(point: Point): Cluster = {
    this._points -= typeTransform(point)
    this
  }

  def ++=(points: TraversableOnce[Types.Type]): Cluster = {
    this._points ++= points.map(typeTransform)
    this
  }

  def --=(points: TraversableOnce[Types.Type]): Cluster = {
    this._points --= points.map(typeTransform)
    this
  }

  def +(point: Types.Type): Cluster = {
    val newPoints = this._points.toSet -/+ typeTransform(point)
    this.copy(points = mutableSetOf(newPoints))
  }

  def -(point: Types.Type): Cluster = {
    val newPoints = this._points.toSet - typeTransform(point)
    this.copy(points = mutableSetOf(newPoints))
  }

  def ++(points: TraversableOnce[Cluster]): Cluster = {
    val newPoints = this._points.toSet --/++ points.map(typeTransform)
    this.copy(points = mutableSetOf(newPoints))
  }

  def --(points: TraversableOnce[Cluster]): Cluster = {
    val newPoints = this._points.toSet -- points.map(typeTransform)
    this.copy(points = mutableSetOf(newPoints))
  }

  def centroid: SyntheticDataType =
    _points.foldLeft(types.EmptySyntheticData()) {
      case (accum, p) =>
        accum + p.syntheticValue
    } / _points.size.toDouble

  /**
    * Calling this method without any point in the cluster is unsafe
    *
    * @return
    */
  def syntheticValue: SyntheticDataType = {

    val syntheticValues = this._points.toList.map(_.syntheticValue)

    sumVectors(syntheticValues, types.EmptySyntheticData())
  }

  override def data: DataType = {

    val values = this._points.toList.map(_.data)

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

  def Empty(implicit types: TypesT): Cluster = Cluster(-1, "empty", Set.empty, 0, None)(types)

  implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticValue

  implicit val toVector: DenseVectorReprOps[Cluster] = new DenseVectorReprOps[Cluster] {

    override def apply(t: Cluster): DenseVector[Double] = clusterToVector(t)

    override def zero(t: Cluster): DenseVector[Double] = t.types.EmptySyntheticData()
  }

  implicit val listToVector: DenseVectorReprOps[List[Cluster]] = new DenseVectorReprOps[List[Cluster]] {

    private def lift(t: List[Cluster]): Cluster = Cluster(-1, "lifted", t, t.headOption.map(_.id).getOrElse(-1), None)(t.head.types)

    override def apply(t: List[Cluster]): DenseVector[Double] = toVector.apply(lift(t))

    override def zero(t: List[Cluster]): DenseVector[Double]  = toVector.zero(t.head)

  }

}
