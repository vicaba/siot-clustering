package algorithm

import algorithm.clusterer.BruteClusterer
import breeze.linalg.{DenseMatrix, DenseVector}
import algorithm.clusterer.BruteClusterer.Settings
import metrics.{Metric, Par}
import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}
import types._

class BruteClustererSpec extends FeatureSpec with GivenWhenThen {

  implicit val types: TypesT = Types4

  val metric = Par.withParAggregate

  feature("cluster.Algorithm") {
    /*scenario("Assigns load profiles minimizing the distance function with perfectly compatible points") {

      Given("4 points that are perfectly compatible")
      val points = List(
        DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
        , DenseMatrix((5.0, 0.0, 5.0, 0.0), (5.0, 0.0, 5.0, 0.0))
        , DenseMatrix((3.0, 0.0, 0.0, 3.0), (4.0, 0.0, 0.0, 4.0))
        , DenseMatrix((0.0, 5.0, 0.0, 5.0), (0.0, 5.0, 0.0, 5.0))
      ).zipWithIndex.map { case (m, idx) =>
        Point(idx, m, None)
      }.toVector

      When("asked to assign each point to a cluster, given 2 clusters")
      val result = Algorithm.run(2, points, Metric.par, 1)

      Then("the two clusters have a PAR of 0")
      result.foreach { cluster =>
        metric(cluster) shouldBe metric.Lowest
      }
    }*/

    /*    scenario("Manual test") {

      val cluster1Points = Set(
        Point(1, DenseMatrix((5.0, 5.0, 0.0, 0.0)), Some(1)),
        Point(2, DenseMatrix((0.0, 0.0, 5.0, 6.0)), Some(1))
      )

      val cluster1 = Cluster(1, "1", cluster1Points)

      val cluster2Points = Set(
        Point(3, DenseMatrix((7.0, 7.0, 0.0, 0.0)), Some(2)),
        Point(4, DenseMatrix((0.0, 0.0, 8.0, 8.0)), Some(2))
      )

      val cluster2 = Cluster(2, "2", cluster2Points)

      info(s"metric c1: ${metric(cluster1)}")
      info(s"metric c2: ${metric(cluster2)}")
      info(s"metric c1+c2: ${metric.aggregateOf(List(cluster1, cluster2))}")

    }*/

    scenario("Assigns load profiles minimizing the distance function") {

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
          Point(idx, m, None)
      }.toVector

      When("asked to assign each point to a cluster, given 2 clusters")
      val runSettings = Settings(1, points, Par.withAverageAggregate, times = 100)
      val result      = BruteClusterer(runSettings)

      Then("the two clusters have a PAR of 1")
      val metricBefore = metric(Cluster.Empty ++ points.map(Point.toCluster))

      result.foreach { cluster => info(cluster.toString)
      }

      result.foreach { cluster =>
        //cluster.points.size should be <= 2
      }

    }

  }
}
