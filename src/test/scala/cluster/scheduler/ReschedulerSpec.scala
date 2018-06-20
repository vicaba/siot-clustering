package cluster.scheduler

import breeze.linalg.{DenseMatrix, DenseVector}
import cluster.Types
import metrics._
import org.scalatest._
import org.scalatest.Matchers._


class ReschedulerSpec extends FeatureSpec with GivenWhenThen {

  val metric = Metric.maxMin

  val vector1 = DenseVector(2.0, 1.0, 2.0, 1.0)
  val vector1Solution = DenseVector(1.0, 2.0, 2.0, 1.0)
  val vector2 = DenseVector(2.0, 1.0, 1.0, 2.0)

  feature("Vector cluster.scheduler.Rescheduler") {
    scenario("schedules vector minimizing distanceFunction") {

      Given("a variable vector")
      val u = vector1.copy

      And("a fixed vector")
      val v = vector2.copy

      When("asked to reschedule")
      val result = Rescheduler.reschedule(u, v, metric)

      Then("the new vector contributes in minimizing overall distanceFunction")
      val originalCompatibility = metric(u + v)
      val betterCompatibility = metric(result.vector + v)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.vector shouldEqual vector1Solution

    }
  }

  val matrix1 = DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
  val matrix1Solution = DenseMatrix((0.0, 3.0, 3.0, 0.0), (4.0, 0.0, 4.0, 0.0))
  val vector = DenseVector(0.0, 7.0, 0.0, 7.0)

  feature("Matrix cluster.scheduler") {
    scenario("schedules matrix minimazing the distanceFunction") {

      Given("a variable matrix")
      val m = matrix1.copy

      And("a fixed vector")
      val u = vector.copy

      When("asked to reschedule")
      val result = Rescheduler.reschedule(m, u, metric)

      Then("the new matrix contributes in minimizing overall distance function")
      val originalCompatibility = metric(Types.synthesizeValues(m) + u)
      val betterCompatibility = metric(Types.synthesizeValues(result.matrix) + u)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.matrix shouldEqual matrix1Solution
    }
  }
}
