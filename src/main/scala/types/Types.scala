package types

import breeze.linalg._
import metrics.DenseVectorReprOps

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

  object Type {

    def toCluster(_type: Type): types.Cluster = _type match {
      case c: types.Cluster => c
      case p: Point => types.Cluster(p.id, p.id.toString, Set(p), 0, None)(p.types)
    }

  }

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

  trait Cluster extends Type {

    type ContainedElement

    def points: scala.collection.Set[ContainedElement]

  }

  object Cluster {

    implicit def clusterToVector(c: Cluster): SyntheticDataType = c.syntheticValue

    implicit val toVector: DenseVectorReprOps[Cluster] = new DenseVectorReprOps[Cluster] {

      override def apply(t: Cluster): DenseVector[Double] = clusterToVector(t)

      override def zero(t: Cluster): DenseVector[Double] = t.types.EmptySyntheticData()
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
