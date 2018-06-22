package clustering.scheduler

import breeze.linalg.{DenseMatrix, DenseVector}
import clustering.scheduler.Rescheduler.VectorResult
import metrics._
import org.scalatest.Matchers._
import org.scalatest._
import types.Types

import scala.annotation.tailrec


class ReschedulerSpec extends FeatureSpec with GivenWhenThen {

  val metric = Metric.par

  def rescheduleTimes(times: Int
    , vectorToReschedule: DenseVector[Double]
    , fixedVector: DenseVector[Double]
    , metric: Metric): List[VectorResult[Double]] = {

    @tailrec
    def _rescheduleTimes(times: Int, vectorToReschedule: DenseVector[Double], accum: List[VectorResult[Double]]):
    List[VectorResult[Double]] = times match {
        case 0 => accum
        case t =>
          val result = Rescheduler.reschedule(vectorToReschedule, fixedVector, metric)
          _rescheduleTimes(t - 1, result.vector, result +: accum)
      }

    _rescheduleTimes(times, vectorToReschedule, List.empty)
  }

  feature("Vector cluster.scheduler.Rescheduler") {
    scenario("schedules vector minimizing distanceFunction (1 change)") {

      Given("a variable vector")
      val u = DenseVector(2.0, 1.0, 2.0, 1.0)

      And("a fixed vector")
      val v = DenseVector(2.0, 1.0, 1.0, 2.0)

      When("asked to reschedule")
      val result = Rescheduler.reschedule(u, v, metric)

      Then("the new vector contributes in minimizing overall distanceFunction")
      val originalCompatibility = metric(u + v)
      val betterCompatibility = metric(result.vector + v)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.vector shouldEqual DenseVector(1.0, 2.0, 2.0, 1.0)

    }

    scenario("schedules vector minimizing distanceFunction (2 changes)") {

      Given("a variable vector")
      val u = DenseVector(2.0, 1.0, 1.0, 2.0)

      And("a fixed vector")
      val v = DenseVector(2.0, 1.0, 1.0, 2.0)

      When("asked to reschedule")
      val result = rescheduleTimes(times = 2, u, v, metric).head

      Then("the new vector contributes in minimizing overall distanceFunction")
      val originalCompatibility = metric(u + v)
      val betterCompatibility = metric(result.vector + v)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.vector shouldEqual DenseVector(1.0, 2.0, 2.0, 1.0)
    }

  }

  feature("Matrix cluster.scheduler") {
    scenario("schedules matrix minimizing the distanceFunction (1 change)") {

      Given("a variable matrix")
      val m = DenseMatrix((0.0, 3.0, 0.0, 3.0), (0.0, 4.0, 0.0, 4.0))

      And("a fixed vector")
      val u = DenseVector(0.0, 7.0, 0.0, 7.0)

      When("asked to reschedule")
      val result = Rescheduler.reschedule(m, u, metric)

      Then("the new matrix contributes in minimizing overall distance function")
      val originalCompatibility = metric(Types.synthesizeValues(m) + u)
      val betterCompatibility = metric(Types.synthesizeValues(result.matrix) + u)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.matrix shouldEqual DenseMatrix((0.0, 3.0, 3.0, 0.0), (4.0, 0.0, 4.0, 0.0))
    }
  }
}
