package algorithm

import algorithm.clusterer.EuclideanClusterer
import algorithm.clusterer.EuclideanClustererSettings
import breeze.linalg.{DenseMatrix, DenseVector}
import metrics.{Metric, Par}
import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}
import types._
import types.clusterer.{DataTypeMetadata, DataTypeMetadata4Columns}
import types.clusterer.mutable.Cluster
import types.clusterer.immutable.Point

class EuclideanClustererSpec extends FeatureSpec with GivenWhenThen {

  implicit val types: DataTypeMetadata = DataTypeMetadata4Columns

  val metric = Par.withParAggregate

  feature("clusterer.EuclideanClusterer") {

    scenario("Does not remove any points from the original dataset") {

      Given("10 compatible points")
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

      When("asked to assign each point to a cluster, given 2 clusters")
      val runSettings = EuclideanClustererSettings(3, points, Par.withAverageAggregate, improveIterations = 100)
      val result      = EuclideanClusterer(runSettings)

      Then("both metrics before and after clustering are the same")

      val metricBefore = metric(Cluster.Empty ++ points.map(Point.toCluster))
      val metricAfter = metric(result)

      metricAfter shouldBe metricBefore

      And("the number of points should be the same before and after lustering")

      Cluster.flatten(result).size shouldBe points.size



    }

  }
}
