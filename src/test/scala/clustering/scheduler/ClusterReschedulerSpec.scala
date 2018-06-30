package clustering.scheduler

import breeze.linalg.{DenseMatrix, DenseVector}
import types._
import metrics._
import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}

class ClusterReschedulerSpec extends FeatureSpec with GivenWhenThen {

  val metric = Metric.par

  val globalPoints = List(
    DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
    , DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
  ).zipWithIndex.map { case (m, idx) =>
    Point(idx, m, Some(0))
  }.toSet

  val globalCluster = Cluster(0, "0", globalPoints)


  feature("Vector cluster.scheduler.ClusterRescheduler") {

    /*scenario("schedules point best that minimizes overall distanceFunction") {

      Given("some points")

      val points = globalPoints

      And("a cluster with those points")

      val cluster = globalCluster

      When("asked to reschedule one point")

      val scheduleResult = ClusterRescheduler.rescheduleOnePoint(cluster, metric)

      Then("the cluster improves distanceFunction")
      val originalCompatibility = metric(cluster)
      val betterCompatibility = metric(scheduleResult.get.cluster)

      betterCompatibility should be < originalCompatibility

    }*/

    scenario("schedules cluster best that minimizes overall distanceFunction") {

      Given("some points")

      val points = globalPoints

      And("a cluster with those points")

      val cluster = globalCluster

      And("a vector solution")
      val vector = DenseVector(7.0, 7.0, 7.0, 7.0)

      When("asked to reschedule the cluster")

      val (betterCluster, scheduleResult) = ClusterRescheduler.rescheduleCluster(cluster, metric, 1)

      Then("the cluster improves distanceFunction")
      val originalCompatibility = metric(cluster)
      val betterCompatibility = metric(betterCluster)

      betterCompatibility should be < originalCompatibility

      And("the syntheticCenter should be equal to the only possible solution")
      betterCluster.syntheticCenter shouldEqual vector
    }
  }

}
