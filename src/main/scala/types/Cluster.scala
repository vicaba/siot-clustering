package types

import breeze.linalg.DenseVector
import metrics.DenseVectorRepr
import types.Types.SyntheticDataType

import scala.annotation.tailrec

case class Cluster(id: Int, name: String, points: Set[Point]) {

  def isEmpty: Boolean = points.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def +(point: Point): Cluster = this.copy(points = (this.points - point) + point.setCluster(id))

  def ++(points: Seq[Point]): Cluster = this.copy(points = (this.points -- points) ++ points.map(_.setCluster(id)))

  def setPoints(points: Seq[Point]): Cluster = this.copy(points = points.map(_.setCluster(id)).toSet)

  def -(point: Point): Cluster = this.copy(points = points - point)

  /**
   * Calling this method without any point in the cluster is unsafe
   * @return
   */
  def syntheticCenter: SyntheticDataType = {

    implicit val types: TypesT = this.points.head.types

    @tailrec
    def sumVectors(remaining: List[SyntheticDataType], accum: SyntheticDataType): SyntheticDataType = remaining match {
      case e :: tail => sumVectors(tail, accum + e)
      case Nil => accum
    }

    val syntheticValues = this.points.toList.map(_.syntheticValue)

    sumVectors(syntheticValues, types.EmptySyntheticData())
    //syntheticValues.fold(EmptySyntheticData())(_ + _)
  }

}

object Cluster {

  import scala.language.implicitConversions


  def Empty: Cluster  = Cluster(-1, "empty", Set.empty)

  implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticCenter

  implicit val toVector: DenseVectorRepr[Cluster] = (t: Cluster) => clusterToVector(t)

}