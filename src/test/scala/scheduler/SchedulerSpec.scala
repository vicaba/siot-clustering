package scheduler

import breeze.linalg.{DenseMatrix, DenseVector}
import cluster.{Scheduler, Types}
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
      val betterCompatibility = Metrics.par(result.vector, v)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.vector shouldEqual vector1Solution

    }
  }

  val matrix1 = DenseMatrix((0.0, 3.0, 3.0, 0.0), (0.0, 4.0, 4.0, 0.0))
  val matrix1Solution = DenseMatrix((0.0, 3.0, 3.0, 0.0), (4.0, 0.0, 4.0, 0.0))
  val vector = DenseVector(0.0, 7.0, 0.0, 7.0)

  feature("Matrix scheduler") {
    scenario("schedules matrix minimazing the distanceFunction") {

      Given("a variable matrix")
      val m = matrix1.copy

      And("a fixed vector")
      val u = vector.copy

      When("asked to reschedule")
      val result = Scheduler.rescheduleMatrix(m, u)

      Then("the new matrix contributes in minimizing overall distance function")
      val originalCompatibility = Metrics.par(Types.synthesizeValues(m), u)
      val betterCompatibility = Metrics.par(Types.synthesizeValues(result.matrix), u)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.matrix shouldEqual matrix1Solution
    }
  }
}
