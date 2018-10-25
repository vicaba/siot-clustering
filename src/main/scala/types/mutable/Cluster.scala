package types.mutable
import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.Types.{DataType, SyntheticDataType}
import types.{Types, TypesT}
import types.ops._

import scala.collection.mutable

class Cluster(val id: Int,
              val name: String,
              val points: mutable.Set[Cluster],
              private var hierarchyLevel: Int = 0,
              private var topLevel: Option[Cluster] = None)(implicit override val types: TypesT)
    extends Types.Cluster {

  override type ContainedElement = Cluster

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

  def +=(point: Cluster): Cluster = {
    this.points += point.setTopLevel(Some(this))
    this
  }

  def ++=(points: Seq[Cluster]): Cluster = {
    points.foreach(this.+=)
    this
  }

  def ++=(points: mutable.Set[Cluster]): Cluster = {
    points.toSeq.foreach(this.+=)
    this
  }

  def -=(point: Cluster): Cluster = {
    this.points -= point
    this
  }

  def setPoints(points: Seq[Cluster]): Cluster = {
    this.points --= this.points
    this.points ++= points
    this
  }

  def setPoints(points: mutable.Set[Cluster]): Cluster =
    this.setPoints(points.toSeq)

  def copy: Cluster = new Cluster(this.id, this.name, this.points, this.hierarchyLevel, this.topLevel)(this.types)

  override def data: DataType = {

    val values = this.points.toList.map(_.data)

    sumPoints(values, types.EmptyData())

  }

  /**
    * Calling this method without any point in the cluster is unsafe
    *
    * @return
    */
  def syntheticValue: SyntheticDataType = {

    val syntheticValues = this.points.toList.map(_.syntheticValue)

    sumVectors(syntheticValues, types.EmptySyntheticData())
  }

  override def centroid: SyntheticDataType =
    points.foldLeft(types.EmptySyntheticData()) {
      case (accum, p) =>
        accum + p.syntheticValue
    } / points.size.toDouble

}

object Cluster {

  import scala.language.implicitConversions

  def Empty(implicit types: TypesT): Cluster = new Cluster(-1, "empty", mutable.Set.empty)(types)

  implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticValue

  implicit val toVector: DenseVectorReprOps[Cluster] = new DenseVectorReprOps[Cluster] {

    override def apply(t: Cluster): DenseVector[Double] = clusterToVector(t)

    override def zero(t: Cluster): DenseVector[Double] = t.types.EmptySyntheticData()
  }

}
