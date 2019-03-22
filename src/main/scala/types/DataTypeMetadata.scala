package types

import breeze.linalg._

trait DataTypeMetadata {

  /**
    * Rows correspond to each appliance, columns correspond to each time interval
    */
  type DataType = DataTypeMetadata.DataType

  type SyntheticDataType = DataTypeMetadata.SyntheticDataType

  val Rows: Int = 1

  val Columns: Int

  def EmptyData(): DataType = DenseMatrix.zeros[Double](Rows, Columns)

  def EmptySyntheticData(): SyntheticDataType = sum(EmptyData(), Axis._0).inner

  def synthesizeValues(values: DataType): SyntheticDataType = sum(values, Axis._0).inner
}

object DataTypeMetadata {

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
