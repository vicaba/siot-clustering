package cluster

import breeze.linalg._

object Types
{

  /**
   * Rows correspond to each appliance, columns correspond to each time interval
   */
  type DataType = DenseMatrix[Int]

  type SyntheticDataType = DenseVector[Int]

  val EmptyData: DataType = DenseMatrix.zeros[Int](25, 5)

  val EmptySyntheticData: SyntheticDataType = sum(EmptyData, Axis._1)

  def synthesizeValues(values: DataType): SyntheticDataType = sum(values, Axis._1)

  case class Point(id: Int, values: DataType, assignedToCluster: Option[Int] = None)
  {

    def setCluster(clusterId: Int): Point = this.copy(assignedToCluster = Some(clusterId))

    def isAssignedToCluster: Boolean = this.assignedToCluster.isDefined

    def setValues(values: DataType): Point = this.copy(values = values)

    def syntheticValue: SyntheticDataType = this.synthesizeValues(this.values)

  }

  case class Cluster(id: Int, name: String, points: scala.Vector[Point])
  {

    def addPoint(point: Point): Cluster = this.copy(points = point +: this.points)

    def addPoints(points: Seq[Point]): Cluster = this.copy(points = points ++: this.points)

    def setPoints(points: Seq[Point]): Cluster = this.copy(points = points.toVector)

    def syntheticCenter: SyntheticDataType = {
      this.points.map(_.syntheticValue).fold(EmptyData)(_ + _)
    }

  }

  object Cluster
  {

    val Empty = Cluster(-1, "empty", scala.Vector.empty)

  }

}
