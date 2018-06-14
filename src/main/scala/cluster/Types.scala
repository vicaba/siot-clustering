package cluster

import breeze.linalg._

object Types {

  /**
   * Rows correspond to each appliance, columns correspond to each time interval
   */
  type DataType = DenseMatrix[Int]

  type SyntheticDataType = DenseVector[Int]

  val Rows = 1

  val Columns = 25

  def EmptyData(): DataType = DenseMatrix.zeros[Int](Rows, Columns)

  def EmptySyntheticData(): SyntheticDataType = sum(EmptyData(), Axis._0).inner

  def synthesizeValues(values: DataType): SyntheticDataType = sum(values, Axis._1)

  case class Point(id: Int, values: DataType, assignedToCluster: Option[Int] = None) {

    def setCluster(clusterId: Int): Point = this.copy(assignedToCluster = Some(clusterId))

    def isAssignedToCluster: Boolean = this.assignedToCluster.isDefined

    def setValues(values: DataType): Point = this.copy(values = values)

    def syntheticValue: SyntheticDataType = synthesizeValues(this.values)

  }

  case class Cluster(id: Int, name: String, points: Set[Point]) {

    def +(point: Point): Cluster = this.copy(points = this.points + point)

    def ++(points: Seq[Point]): Cluster = this.copy(points = this.points ++ points)

    def setPoints(points: Seq[Point]): Cluster = this.copy(points = points.toSet)

    def -(point: Point): Cluster = this.copy(points = points - point)

    def syntheticCenter: SyntheticDataType = {
      this.points.map(_.syntheticValue).fold(EmptySyntheticData())(_ + _)
    }

  }

  object Cluster {

    val Empty = Cluster(-1, "empty", Set.empty)

  }

}
