package algorithm.scheduler

import breeze.linalg.{DenseMatrix, DenseVector}
import algorithm.scheduler.ClusterRescheduler.PointChange
import types._
import metrics._
import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}

import scala.annotation.tailrec

class ClusterReschedulerSpec extends FeatureSpec with GivenWhenThen {

  implicit val types = Types4

  val metric = Metric.par

  def rescheduleTimes(times: Int
    , clusterToReschedule: Cluster
  , metric: Metric): List[PointChange] = {

    @tailrec
    def _rescheduleTimes(times: Int, clusterToReschedule: Cluster, accum: List[PointChange]): List[PointChange] = {
      times match {
        case 0 => accum
        case t =>
          val result = ClusterRescheduler.rescheduleOnePoint(clusterToReschedule, metric)
          if (result.isDefined) {
            _rescheduleTimes(t - 1, result.get.cluster, result.get +: accum)
          } else {
            _rescheduleTimes(t - 1, clusterToReschedule, accum)
          }
      }
    }

    _rescheduleTimes(times, clusterToReschedule, List.empty)

  }

  val globalPoints = List(
    DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
    , DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
  ).zipWithIndex.map { case (m, idx) =>
    Point(idx, m, Some(0))
  }.toSet

  val globalCluster = Cluster(0, "0", globalPoints)

  feature("Vector cluster.scheduler.ClusterRescheduler") {

    scenario("schedules point best that minimizes overall distanceFunction") {

      Given("some points")

      val points = globalPoints

      And("a cluster with those points")

      val cluster = globalCluster

      When("asked to reschedule one point")

      val scheduleResult = rescheduleTimes(11, cluster, metric)

      Then("the cluster improves distanceFunction")
      val originalCompatibility = metric(cluster)
      val betterCompatibility = metric(scheduleResult.head.cluster)

      betterCompatibility should be < originalCompatibility

    }


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

    }

  }

}
