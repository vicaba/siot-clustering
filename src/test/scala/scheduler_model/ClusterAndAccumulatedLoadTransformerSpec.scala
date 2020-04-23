package scheduler_model

import breeze.linalg.DenseMatrix
import metrics.Metric
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import org.scalatest.Inspectors._
import _root_.reader.TemplateForSyntheticProfilesReader
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster
import types.clusterer.{DataTypeMetadata, DataTypeMetadata4Columns, Type}

class ClusterAndAccumulatedLoadTransformerSpec extends FeatureSpec with GivenWhenThen {

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

  feature("Conversion between one type and the other keeps basic characteristics") {

    scenario("Having a set of clusters, conversion to load model and back to clusters keeps the same characteristics") {

      Given("some points")

      When("splitting them into two clusters")

      val (group1, group2) = points.splitAt(points.size / 2)

      val c1 = Cluster(-1, "-1", group1, 0, None)
      val c2 = Cluster(-4, "-1", group2, 0, None)

      val clusters = List(c1, c2)

      And("transforming them to accumulatedLoads")

      val accumulatedLoads = ClusterAndAccumulatedLoadTransformer(clusters, types)

      Then("both should have the same PAR")

      val clustersMetric         = Metric.par(clusters)
      val accumulatedLoadsMetric = Metric.par(accumulatedLoads)

      accumulatedLoadsMetric shouldBe clustersMetric

      When("transforming accumulated loads back to clusters")

      val clusters2 = ClusterAndAccumulatedLoadTransformer.reverse(accumulatedLoads, clusters.head.dataTypeMetadata)

      Then("the number of clusters created should be equal")

      clusters2.size shouldBe clusters.size

      And("clusters with the same id should keep the same number of points per cluster")

      And("the same point id and synthetic value per point")

      val groupedClustersById = (clusters ++ clusters2).groupBy(_.id)

      groupedClustersById.foreach {
        case (_, gClusters: Iterable[Cluster]) =>
          // Cluster before rescheduling and cluster after rescheduling have the same size
          // (they all have the size of one of them)
          all(gClusters.map(_.size)) shouldBe (gClusters.head.size)

          // For each list of points
          forAll(gClusters.map(_.points)) { points: scala.collection.Set[Type] =>
            // Double-check that the cluster.size is the same by comparing the length of the list of points
            points.size shouldBe gClusters.head.points.size

            // Pair any list of points with another list of points
            (points.toList ++ gClusters.head.points.toList).groupBy(_.id).map {
              case (_, gPoints) =>
                // Any id of the list of points should be equal to the id of another list of points
                // (assuming that both lists are ordered)
                all(gPoints.map(_.id)) shouldBe (gPoints.head.id)

                // Any syntheticValue of the list of points should be equal to the syntheticValue of another list of points
                // (assuming that both lists are ordered)
                all(gPoints.map(_.syntheticValue)) shouldBe (gPoints.head.syntheticValue)

                // Both lists should contain the same elements
                all(gPoints) shouldBe gPoints.head

            }

          }

      }

      And("there should not be any cluster that did not exist before (different id)")

      groupedClustersById.forall { case (_, clusters) => clusters.size > 1 } shouldBe true

      And("both should have the same PAR")

      val clusters2Metric = Metric.par(clusters2)

      clusters2Metric shouldBe accumulatedLoadsMetric

    }

  }

}
