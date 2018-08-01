package types

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.Types.SyntheticDataType

import scala.annotation.tailrec

case class Cluster(id: Int, name: String, points: Set[Point])(implicit val types: TypesT) {

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

    @tailrec
    def sumVectors(remaining: List[SyntheticDataType], accum: SyntheticDataType): SyntheticDataType = remaining match {
      case e :: tail => sumVectors(tail, accum + e)
      case Nil       => accum
    }

    val syntheticValues = this.points.toList.map(_.syntheticValue)

    sumVectors(syntheticValues, types.EmptySyntheticData())
  }

}

object Cluster {

  import scala.language.implicitConversions

  def Empty(implicit types: TypesT): Cluster = Cluster(-1, "empty", Set.empty)(types)

  implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticCenter

  implicit val toVector: DenseVectorReprOps[Cluster] = new DenseVectorReprOps[Cluster] {

    override def apply(t: Cluster): DenseVector[Double] = clusterToVector(t)

    override def zero(t: Cluster): DenseVector[Double] = t.types.EmptySyntheticData()
  }

}
