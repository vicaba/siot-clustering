package algorithm

import algorithm.clusterer.EuclideanClusterer
import algorithm.clusterer.EuclideanClustererSettings
import breeze.linalg.{DenseMatrix, DenseVector}
import collection.shuffler.{Keep, Random}
import metrics.{Metric, Par}
import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}
import types._
import types.clusterer.{DataTypeMetadata, DataTypeMetadata4Columns}
import types.clusterer.mutable.Cluster
import types.clusterer.immutable.Point

class EuclideanClustererSpec extends FeatureSpec with GivenWhenThen {

  implicit val types: DataTypeMetadata = DataTypeMetadata4Columns

  val points = List(
    DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0)),
    DenseMatrix((5.0, 0.0, 5.0, 0.0), (5.0, 0.0, 5.0, 0.0)),
    DenseMatrix((3.0, 0.0, 0.0, 3.0), (4.0, 0.0, 0.0, 4.0)),
    DenseMatrix((0.0, 5.0, 0.0, 5.0), (0.0, 5.0, 0.0, 5.0)),
    DenseMatrix((1.0, 5.0, 5.0, 5.0), (0.0, 2.0, 3.0, 5.0)),
    DenseMatrix((8.0, 1.0, 0.0, 0.0), (0.0, 1.0, 0.0, 1.0)),
    DenseMatrix((1.0, 0.0, 2.0, 0.0)),
    DenseMatrix((4.0, 3.0, 1.0, 7.0)),
    DenseMatrix((10.0, 10.0, 10.0, 10.0), (1.0, 1.0, 1.0, 1.0), (17.0, 0.0, 1.0, 6.0)),
    DenseMatrix((0.0, 12.0, 12.0, 12.0))
  ).zipWithIndex.map {
    case (m, idx) =>
      Point(idx, m, Nil, None)
  }.toVector

  val metric = Par.withParAggregate

  feature("clusterer.EuclideanClusterer") {

    scenario("Does not remove any points from the original dataset") {

      Given("10 compatible points")
      // Stored in val points

      When("asked to assign each point to a cluster, given 3 clusters")
      val runSettings = EuclideanClustererSettings(3, points, Par.withAverageAggregate, improveIterations = 100)
      val result      = EuclideanClusterer(runSettings)

      Then("both metrics before and after clustering are the same")

      val metricBefore = metric(Cluster.Empty ++ points.map(Point.toCluster))
      val metricAfter  = metric(result)

      metricAfter shouldBe metricBefore

      And("the number of points should be the same before and after lustering")

      Cluster.flatten(result).size shouldBe points.size

    }

    scenario("If points ARE shuffled, the result is non-deterministic") {

      Given("10 compatible points")
      // Stored in val points

      When("asked to assign each point to a cluster among 3 clusters")
      val runSettings = EuclideanClustererSettings(3,
                                                   points,
                                                   Par.withAverageAggregate,
                                                   improveIterations = 1,
                                                   Random(scala.util.Random))

      val result  = EuclideanClusterer(runSettings)
      val result2 = EuclideanClusterer(runSettings)

      Then("both results are NOT the same")

      Cluster.flattenAsList(result).map(_.id) shouldNot be(Cluster.flattenAsList(result2).map(_.id))

    }

    scenario("If points are NOT shuffled, the result is deterministic") {

      Given("10 compatible points")
      // Stored in val points

      When("asked to assign each point to a cluster among 3 clusters")
      val runSettings = EuclideanClustererSettings(3, points, Par.withAverageAggregate, improveIterations = 1, Keep)

      val result  = EuclideanClusterer(runSettings)
      val result2 = EuclideanClusterer(runSettings)

      Then("both results are the same")

      Cluster.flattenAsList(result).map(_.id) shouldBe Cluster.flattenAsList(result2).map(_.id)

    }

  }
}
