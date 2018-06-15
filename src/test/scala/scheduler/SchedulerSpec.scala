package scheduler

import breeze.linalg.DenseVector
import cluster.Scheduler
import metrics.Metrics
import org.scalatest._
import org.scalatest.Matchers._


class SchedulerSpec extends FeatureSpec with GivenWhenThen {

  val vector1 = DenseVector(2.0, 1.0, 2.0, 1.0)
  val vector1Solution = DenseVector(1.0, 2.0, 2.0, 1.0)
  val vector2 = DenseVector(2.0, 1.0, 1.0, 2.0)

  feature("Vector scheduler") {
    scenario("schedules vector minimizing distanceFunction") {

      Given("a variable vector")
      val u = vector1.copy

      And("a fixed vector")
      val v = vector2.copy

      When("asked to reschedule")
      val result = Scheduler.rescheduleVector(u, v)

      Then("the new vector contributes in minimizing overall distanceFunction")
      val originalCompatibility = Metrics.par(u, v)

      val betterCompatibility = Metrics.par(result, v)

      betterCompatibility should be < originalCompatibility
    }

    scenario("schedules vector minimizing distanceFunction to the preprocessed solution") {

      Given("a variable vector")
      val u = vector1.copy

      And("a fixed vector")
      val v = vector2.copy

      When("asked to reschedule")
      val result = Scheduler.rescheduleVector(u, v)

      Then("the new vector is the only solution (in this simple case) that minimizes overall distanceFunction")
      val originalCompatibility = Metrics.par(u, v)
      val betterCompatibility = Metrics.par(result, v)

      result shouldEqual vector1Solution
    }
  }

}
