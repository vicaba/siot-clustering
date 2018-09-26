package types

import breeze.linalg._

import scala.annotation.tailrec

trait TypesT {

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

  trait Type {

    def id: Int

    def data: DataType

    def syntheticValue: SyntheticDataType

    def centroid: SyntheticDataType

    def types: TypesT

    @tailrec
    final def sumPoints(remaining: List[DataType], accum: DataType): DataType = remaining match {
      case e :: tail => sumPoints(tail, accum + e)
      case Nil       => accum
    }

    @tailrec
    final def sumVectors(remaining: List[SyntheticDataType], accum: SyntheticDataType): SyntheticDataType =
      remaining match {
        case e :: tail => sumVectors(tail, accum + e)
        case Nil       => accum
      }


  }

  /**
    * Rows correspond to each appliance, columns correspond to each time interval
    */
  type DataType = DenseMatrix[Double]

  type SyntheticDataType = DenseVector[Double]

  def EmptyData()(implicit types: TypesT): DataType = DenseMatrix.zeros[Double](types.Rows, types.Columns)

  def EmptySyntheticData()(implicit types: TypesT): SyntheticDataType = sum(EmptyData(), Axis._0).inner

  def synthesizeValues(values: DataType): SyntheticDataType = sum(values, Axis._0).inner

}

object Types2 extends TypesT {
  override val Columns: Int = 2
}

object Types4 extends TypesT {
  override val Columns: Int = 4
}

object Types24 extends TypesT {
  override val Columns: Int = 24
}

object Types67_24 extends TypesT {
  override val Columns: Int = 24
}
