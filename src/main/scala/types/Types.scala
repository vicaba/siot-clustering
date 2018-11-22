package types

import breeze.linalg._
import metrics.DenseVectorReprOps
import types.immutable.Point

import scala.annotation.tailrec

trait DataTypeMetadata {

  /**
    * Rows correspond to each appliance, columns correspond to each time interval
    */
  type DataType = DenseMatrix[Double]

  type SyntheticDataType = DenseVector[Double]

  val Rows = 1

  val Columns: Int

  def EmptyData(): DataType = DenseMatrix.zeros[Double](Rows, Columns)

  def EmptySyntheticData(): SyntheticDataType = sum(EmptyData(), Axis._0).inner

  def synthesizeValues(values: DataType): SyntheticDataType = sum(values, Axis._0).inner
}

object Types {

  trait Cluster extends Type {

    type ContainedElement

    def points: scala.collection.Set[ContainedElement]

    override def toString: String = s"Cluster($id, $size, $points)"

  }

  object Cluster {

    implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticValue

    implicit val toVector: DenseVectorReprOps[Cluster] = new DenseVectorReprOps[Cluster] {

      override def apply(t: Cluster): DenseVector[Double] = clusterToVector(t)

      override def zero(t: Cluster): DenseVector[Double] = t.dataTypeMetadata.EmptySyntheticData()
    }

  }

  /**
    * Rows correspond to each appliance, columns correspond to each time interval
    */
  type DataType = DenseMatrix[Double]

  type SyntheticDataType = DenseVector[Double]

  def EmptyData()(implicit types: DataTypeMetadata): DataType = DenseMatrix.zeros[Double](types.Rows, types.Columns)

  def EmptySyntheticData()(implicit types: DataTypeMetadata): SyntheticDataType = sum(EmptyData(), Axis._0).inner

  def synthesizeValues(values: DataType): SyntheticDataType = sum(values, Axis._0).inner

}

object DataTypeMetadata2Columns extends DataTypeMetadata {
  override val Columns: Int = 2
}

object DataTypeMetadata4Columns extends DataTypeMetadata {
  override val Columns: Int = 4
}

object DataTypeMetadata24Columns extends DataTypeMetadata {
  override val Columns: Int = 24
}

object Types67_24 extends DataTypeMetadata {
  override val Columns: Int = 24
}
