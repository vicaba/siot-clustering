package types

import breeze.linalg._

object Types {

  /**
   * Rows correspond to each appliance, columns correspond to each time interval
   */
  type DataType = DenseMatrix[Double]

  type SyntheticDataType = DenseVector[Double]

  val Rows = 1

  val Columns = 4

  def EmptyData(): DataType = DenseMatrix.zeros[Double](Rows, Columns)

  def EmptySyntheticData(): SyntheticDataType = sum(EmptyData(), Axis._0).inner

  def synthesizeValues(values: DataType): SyntheticDataType = sum(values, Axis._0).inner

}
