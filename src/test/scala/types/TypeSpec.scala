package types

import algorithm.BatchRunSettingsBuilder
import breeze.linalg.{DenseMatrix, sum}
import metrics.Par
import org.scalatest.FlatSpec
import reader.TemplateForSyntheticProfilesReader
import types.clusterer.{DataTypeMetadata, DataTypeMetadata4Columns, Type}
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster
import org.scalatest.Matchers._

class TypeSpec extends FlatSpec {

  implicit val types: DataTypeMetadata = DataTypeMetadata4Columns

  val points: scala.Vector[Point] = List(
    DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0)),
    DenseMatrix((5.0, 0.0, 5.0, 0.0), (5.0, 0.0, 5.0, 0.0)),
    DenseMatrix((3.0, 0.0, 0.0, 3.0), (4.0, 0.0, 0.0, 4.0)),
    /*DenseMatrix((0.0, 5.0, 0.0, 5.0), (0.0, 5.0, 0.0, 5.0)),
    DenseMatrix((1.0, 5.0, 5.0, 5.0), (0.0, 2.0, 3.0, 5.0)),
    DenseMatrix((8.0, 1.0, 0.0, 0.0), (0.0, 1.0, 0.0, 1.0)),
    DenseMatrix((1.0, 0.0, 2.0, 0.0)),
    DenseMatrix((4.0, 3.0, 1.0, 7.0)),
    DenseMatrix((10.0, 10.0, 10.0, 10.0), (1.0, 1.0, 1.0, 1.0), (17.0, 0.0, 1.0, 6.0)),
    DenseMatrix((0.0, 12.0, 12.0, 12.0))*/
  ).zipWithIndex.map {
    case (m, idx) =>
      Point(idx, m, List(TemplateForSyntheticProfilesReader.FlexibleLoads.head, "FixedLoad"), None)
  }.toVector


  val (group1, group2) = points.splitAt(points.size/2)

  val c1 = Cluster(-1, "-1", group1, 0, None)
  val c2 = Cluster(-4, "-1", group2, 0, None)

  val clusters = List(c1, c2)

  val output: List[Cluster] = Type.deepCopy(clusters).asInstanceOf[List[Cluster]]

  val totalEnergyBeforeCopying: Double = sum(sum(clusters.map(_.syntheticValue)))
  val totalEnergyAfterCopying: Double = sum(sum(output.map(_.syntheticValue)))

  totalEnergyBeforeCopying shouldBe totalEnergyAfterCopying

}
