package clustering

import breeze.linalg.{DenseMatrix, DenseVector}
import metrics.Metric
import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}
import types.{Cluster, Point, Types}

class AlgorithmSpec extends FeatureSpec with GivenWhenThen {

  val globalPoints = List(
    DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
    , DenseMatrix((5.0, 0.0 , 5.0, 0.0), (5.0, 0.0, 5.0, 0.0))
    , DenseMatrix((3.0, 0.0 , 0.0, 3.0), (4.0, 0.0, 0.0, 4.0))
    , DenseMatrix((0.0, 5.0, 0.0, 5.0), (0.0, 5.0, 0.0, 5.0))
  ).zipWithIndex.map { case (m, idx) =>
    Point(idx, m, None)
  }.toVector

  feature("cluster.Algorithm") {
    scenario("Assigns load profiles minimizing the distance function") {

      Given("4 points that are perfectly compatible")
      val points = globalPoints

      When("asked to assign each point to a cluster, given 2 clusters")
      val result = Algorithm.run(2, points, Metric.par, 1)

      Then("the two clusters have a PAR of 1")
      result.foreach { cluster =>
        info(cluster.toString)
      }
      result.foreach { cluster =>
        Metric.par(cluster) shouldBe(1.0)
      }
    }
  }
}
