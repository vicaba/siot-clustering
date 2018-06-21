package types

import types.Types.SyntheticDataType

import scala.annotation.tailrec

case class Cluster(id: Int, name: String, points: Set[Point]) {

  def +(point: Point): Cluster = this.copy(points = (this.points - point) + point)

  def ++(points: Seq[Point]): Cluster = this.copy(points = (this.points -- points) ++ points)

  def setPoints(points: Seq[Point]): Cluster = this.copy(points = points.toSet)

  def -(point: Point): Cluster = this.copy(points = points - point)

  /**
   * Calling this method without any point in the cluster is unsafe
   * @return
   */
  def syntheticCenter: SyntheticDataType = {

    @tailrec
    def sumVectors(remaining: List[SyntheticDataType], accum: SyntheticDataType): SyntheticDataType = remaining match {
      case e :: tail => sumVectors(tail, accum + e)
      case Nil => accum
    }

    val syntheticValues = this.points.toList.map(_.syntheticValue)

    sumVectors(syntheticValues, syntheticValues.head - syntheticValues.head)
    //syntheticValues.fold(EmptySyntheticData())(_ + _)
  }

}

object Cluster {

  import scala.language.implicitConversions


  val Empty = Cluster(-1, "empty", Set.empty)

  implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticCenter

}