package clustering

import breeze.linalg.{DenseMatrix, DenseVector}
import clustering.Algorithm.Run
import metrics.Metric
import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}
import types.{Cluster, Point, Types}

class AlgorithmSpec extends FeatureSpec with GivenWhenThen {

  val metric = Metric.par


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


    scenario("Assigns load profiles minimizing the distance function") {

      Given("10 compatible points")
      val points = List(
        DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
        , DenseMatrix((5.0, 0.0 , 5.0, 0.0), (5.0, 0.0, 5.0, 0.0))
        , DenseMatrix((3.0, 0.0 , 0.0, 3.0), (4.0, 0.0, 0.0, 4.0))
        , DenseMatrix((0.0, 5.0, 0.0, 5.0), (0.0, 5.0, 0.0, 5.0))
        , DenseMatrix((1.0, 5.0, 5.0, 5.0), (0.0, 2.0, 3.0, 5.0))
        , DenseMatrix((8.0, 1.0, 0.0, 0.0), (0.0, 1.0, 0.0, 1.0))
        , DenseMatrix((1.0, 0.0, 2.0, 0.0))
        , DenseMatrix((4.0, 3.0, 1.0, 7.0))
        , DenseMatrix((10.0, 10.0, 10.0, 10.0), (1.0, 1.0, 1.0, 1.0), (17.0, 0.0, 1.0, 6.0))
        , DenseMatrix((0.0, 12.0, 12.0, 12.0))
      ).zipWithIndex.map { case (m, idx) =>
        Point(idx, m, None)
      }.toVector

      When("asked to assign each point to a cluster, given 2 clusters")
      val run = Run(5, points, Metric.par, 0.5)
      val result = Algorithm.runIterative(run, 100)

      Then("the two clusters have a PAR of 1")
      val metricBefore = metric(Cluster.Empty ++ points)

      result.foreach { cluster =>
        info(cluster.toString)
      }

      result.foreach { cluster =>
        cluster.points.size should be < 2
      }

    }

  }
}
