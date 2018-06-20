package cluster

import breeze.linalg._

import scala.annotation.tailrec

object Types {

  /**
   * Rows correspond to each appliance, columns correspond to each time interval
   */
  type DataType = DenseMatrix[Double]

  type SyntheticDataType = DenseVector[Double]

  val Rows = 1

  val Columns = 25

  def EmptyData(): DataType = DenseMatrix.zeros[Double](Rows, Columns)

  def EmptySyntheticData(): SyntheticDataType = sum(EmptyData(), Axis._0).inner

  def synthesizeValues(values: DataType): SyntheticDataType = sum(values, Axis._0).inner

  case class Point(id: Int, values: DataType, assignedToCluster: Option[Int] = None) {

    override def equals(obj: scala.Any): Boolean = obj match {
      case p: Point => this.id == p.id
      case _ => false
    }

    def setCluster(clusterId: Int): Point = this.copy(assignedToCluster = Some(clusterId))

    def isAssignedToCluster: Boolean = this.assignedToCluster.isDefined

    def setValues(values: DataType): Point = this.copy(values = values)

    def syntheticValue: SyntheticDataType = synthesizeValues(this.values)

  }

  case class Cluster(id: Int, name: String, points: Set[Point]) {

    def +(point: Point): Cluster = this.copy(points = (this.points - point) + point)

    def ++(points: Seq[Point]): Cluster = this.copy(points = (this.points -- points) ++ points)

    def setPoints(points: Seq[Point]): Cluster = this.copy(points = points.toSet)

    def -(point: Point): Cluster = this.copy(points = points - point)

    /**
     * Calling this method without any point is unsafe
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

}
