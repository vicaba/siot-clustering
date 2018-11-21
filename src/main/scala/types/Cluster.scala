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
                            private var _hierarchyLevel: Int = 0,
                            private var _topLevel: Option[Cluster] = None)(implicit override val types: TypesT)
    extends Types.Cluster {

  override type ThisType = Cluster

  override type ContainedElement = Types.Type

  override def deepCopy(): ThisType = this.copy()

  def copy(id: Int = this.id,
           name: String = this.name,
           points: TraversableOnce[Types.Type] = this._points,
           hierarchylevel: Int = this._hierarchyLevel,
           topLevel: Option[Cluster] = this._topLevel): Cluster = {
    new Cluster(id, name, mutableSetOf(points.map(_.deepCopy())), _hierarchyLevel, topLevel)(types)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: Cluster => this.name == c.name
    case _          => false
  }

  override def hashCode(): Int = this.id

  def isEmpty: Boolean = _points.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def setPoints(points: Traversable[Types.Type]): Cluster = {
    (this --= points) ++= points.map(typeTransform)
    this
  }

  override def points: collection.Set[Types.Type] = _points.toSet

  def hierarchyLevel_=(_hierarchyLevel: Int): Cluster = {
    this._hierarchyLevel = _hierarchyLevel
    this
  }

  def hierarchyLevel: Int = _hierarchyLevel

  def topLevel_=(_topLevel: Option[Cluster]): Cluster = {
    this._topLevel = _topLevel
    this
  }

  def topLevel: Option[Cluster] = _topLevel

  def +=(point: Types.Type): Cluster = {
    (this._points -= typeTransform(point)) += typeTransform(point)
    this
  }

  def -=(point: Point): Cluster = {
    this._points -= typeTransform(point)
    this
  }

  def ++=(points: Traversable[Types.Type]): Cluster = {
    this._points --/++= points.map(typeTransform)
    this
  }

  def --=(points: Traversable[Types.Type]): Cluster = {
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

  def ++(points: Traversable[Cluster]): Cluster = {
    val newPoints = this._points.toSet --/++ points.map(typeTransform)
    this.copy(points = mutableSetOf(newPoints))
  }

  def --(points: Traversable[Cluster]): Cluster = {
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

  override def size: Int = _points.foldLeft(0)(_ + _.size)

  private def typeTransform(_type: Types.Type): Types.Type = _type match {
    case p: Point   => p.setCluster(this)
    case c: Cluster => c.topLevel_=(Some(this)).hierarchyLevel_=(this._hierarchyLevel - 1)
  }

  def mutableSetOf[A](s: TraversableOnce[A]): scala.collection.mutable.Set[A] =
    Cluster.mutableSetOf(s)

}

object Cluster {

  def mutableSetOf[A](s: TraversableOnce[A]): scala.collection.mutable.Set[A] =
    new scala.collection.mutable.HashSet[A]() ++= s

  import scala.language.implicitConversions

  def apply(id: Int, name: String, points: Traversable[Types.Type], hierarchyLevel: Int, topLevel: Option[Cluster])(
      implicit types: TypesT): Cluster = {
    val c = new Cluster(id, name, mutableSetOf(Set.empty), hierarchyLevel, topLevel)(types)
    c ++= points
  }

  def Empty(implicit types: TypesT): Cluster = Cluster(-1, "empty", Set.empty, 0, None)(types)

  implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticValue

  implicit val toVector: DenseVectorReprOps[Cluster] = new DenseVectorReprOps[Cluster] {

    override def apply(t: Cluster): DenseVector[Double] = clusterToVector(t)

    override def zero(t: Cluster): DenseVector[Double] = t.types.EmptySyntheticData()
  }

  implicit val listToVector: DenseVectorReprOps[List[Cluster]] = new DenseVectorReprOps[List[Cluster]] {

    private def lift(t: List[Cluster]): Cluster =
      Cluster(-1, "lifted", t, t.headOption.map(_.id).getOrElse(-1), None)(t.head.types)

    override def apply(t: List[Cluster]): DenseVector[Double] = toVector.apply(lift(t))

    override def zero(t: List[Cluster]): DenseVector[Double] = toVector.zero(t.head)

  }

  def flatten(cl: TraversableOnce[Cluster]): Set[Point] = {

    @tailrec
    def _flatten(types: List[Types.Type], accum: List[Point]): List[Point] = types match {
      case Nil => accum
      case h :: tail =>
        h match {
          case p: Point   => _flatten(tail, p :: accum)
          case c: Cluster => _flatten(c.points.toList ::: tail, accum)
        }
    }

    _flatten(cl.toList, Nil).toSet

  }

  def flatten(c: Cluster): Set[Point] = flatten(List(c))

  def traverseAndFindFittest(cl: List[Cluster], p: Cluster => Double): Option[(Double, Cluster)] = {

    @tailrec
    def _traverseAndFindFittest(types: List[Types.Type], best: (Double, Cluster)): (Double, Cluster) = types match {
      case Nil => best
      case h :: tail =>
        h match {
          case c: Cluster =>
            val fitValue = p(c)
            if (fitValue > best._1) {
              _traverseAndFindFittest(c.points.toList ::: tail, (fitValue, c))
            } else _traverseAndFindFittest(c.points.toList ::: tail, best)
          case _ => _traverseAndFindFittest(tail, best)
        }
    }

    cl.headOption.map(c => _traverseAndFindFittest(cl, (p(c), c)))

  }

  def traverseAndFindFittest(c: Cluster, p: Cluster => Double): Option[(Double, Cluster)] = traverseAndFindFittest(List(c), p)


}
