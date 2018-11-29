package algorithm.scheduler

import breeze.linalg.{DenseMatrix, DenseVector}
import algorithm.scheduler.Rescheduler.{MatrixResult, VectorResult}
import metrics._
import org.scalatest.Matchers._
import org.scalatest._
import types.{DataTypeMetadata, DataTypeMetadata4Columns}

import scala.annotation.tailrec

class ReschedulerSpec extends FeatureSpec with GivenWhenThen {

  implicit val types = DataTypeMetadata4Columns

  val metric = Metric.par

  def rescheduleTimes(times: Int,
                      vectorToReschedule: DenseVector[Double],
                      fixedVector: DenseVector[Double],
                      metric: Metric): List[VectorResult[Double]] = {

    @tailrec
    def _rescheduleTimes(times: Int,
                         vectorToReschedule: DenseVector[Double],
                         accum: List[VectorResult[Double]]): List[VectorResult[Double]] = times match {
      case 0 => accum
      case t =>
        val result = Rescheduler.reschedule(vectorToReschedule, fixedVector, metric)
        _rescheduleTimes(t - 1, result.vector, result +: accum)
    }

    _rescheduleTimes(times, vectorToReschedule, List.empty)
  }

  def rescheduleTimes(times: Int,
                      matrixToReschedule: DenseMatrix[Double],
                      fixedVector: DenseVector[Double],
                      metric: Metric): List[MatrixResult[Double]] = {

    @tailrec
    def _rescheduleTimes(times: Int,
                         matrixToReschedule: DenseMatrix[Double],
                         accum: List[MatrixResult[Double]]): List[MatrixResult[Double]] = times match {
      case 0 => accum
      case t =>
        val result = Rescheduler.reschedule(matrixToReschedule, fixedVector, metric)
        if (result.isDefined) {
          _rescheduleTimes(t - 1, result.get.matrix, result.get +: accum)
        } else {
          _rescheduleTimes(0, matrixToReschedule, accum)
        }
    }

    _rescheduleTimes(times, matrixToReschedule, List.empty)
  }

  feature("Vector rescheduler minimizes distance function") {

    scenario("schedules vector minimizing distance function (1 change)") {

      Given("a variable vector")
      val u = DenseVector(2.0, 1.0, 2.0, 1.0)

      And("a fixed vector")
      val v = DenseVector(2.0, 1.0, 1.0, 2.0)

      When("asked to reschedule 1 time")
      val result = Rescheduler.reschedule(u, v, metric)

      Then("the new vector contributes in minimizing overall distance function")
      val originalCompatibility = metric(u + v)
      val betterCompatibility   = metric(result.vector + v)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.vector shouldEqual DenseVector(1.0, 2.0, 2.0, 1.0)

    }

    scenario("schedules vector minimizing distance function (2 changes)") {

      Given("a variable vector")
      val u = DenseVector(2.0, 1.0, 1.0, 2.0)

      And("a fixed vector")
      val v = DenseVector(2.0, 1.0, 1.0, 2.0)

      When("asked to reschedule 2 times")
      val result = rescheduleTimes(times = 2, u, v, metric).head

      Then("the new vector contributes in minimizing overall distance function")
      val originalCompatibility = metric(u + v)
      val betterCompatibility   = metric(result.vector + v)

      betterCompatibility should be < originalCompatibility

      And("the new vector should be equal to the only possible solution")
      result.vector shouldEqual DenseVector(1.0, 2.0, 2.0, 1.0)
    }

    scenario("schedules vector minimizing distance function when the vector to reschedule has lower distance function") {
      Given("a variable vector")
      val u = DenseVector(0.0, 6.0, 1.0, 7.0)

      And("a fixed vector")
      val v = DenseVector(6.0, 0.0, 7.0, 0.0)

      When("asked to reschedule 2 times")
      val result = rescheduleTimes(times = 1, u, v, metric).head

      Then("the new vector contributes in minimizing overall distance function")
      val originalCompatibility = metric(u + v)
      val betterCompatibility   = metric(result.vector + v)

      betterCompatibility should be < originalCompatibility

    }

  }

  feature("Matrix rescheduler minimizes distance function") {

    scenario("schedules matrix minimizing the distance function (2 changes)") {

      Given("a variable matrix")
      val m = DenseMatrix((0.0, 3.0, 0.0, 3.0), (0.0, 4.0, 0.0, 4.0))

      And("a fixed vector")
      val u = DenseVector(0.0, 7.0, 0.0, 7.0)

      When("asked to reschedule 2 times")
      val result = rescheduleTimes(times = 4, m, u, metric).head

      Then("the new matrix contributes in minimizing overall distance function")
      val originalCompatibility = metric(DataTypeMetadata.synthesizeValues(m) + u)
      val betterCompatibility   = metric(DataTypeMetadata.synthesizeValues(result.matrix) + u)

      betterCompatibility should be < originalCompatibility

    }

    scenario("Schedules matrix minimizing the distance function (4 changes)") {

      Given("a variable matrix")
      val m = DenseMatrix((0.0, 3.0, 0.0, 3.0), (0.0, 4.0, 0.0, 4.0))

      And("a fixed vector")
      val u = DenseVector(0.0, 7.0, 0.0, 7.0)

      When("asked to reschedule 4 times")
      val result = rescheduleTimes(times = 4, m, u, metric).head

      Then("the new matrix contributes in minimizing overall distance function")
      val betterCompatibility = metric(DataTypeMetadata.synthesizeValues(result.matrix) + u)

      betterCompatibility should equal(metric.Lowest)

      And("the new vector should be equal to the only possible solution")
      result.matrix shouldEqual DenseMatrix((3.0, 0.0, 3.0, 0.0), (4.0, 0.0, 4.0, 0.0))
    }
    // TODO: Current algorithm doesn't improve this
    /*scenario("Schedules matrix minimizing the distance function (4 changes). sum(matrix) == vector. Reorder matrix") {

      Given("a variable matrix")
      val m = DenseMatrix((3.0, 0.0, 0.0, 3.0), (0.0, 4.0, 4.0, 0.0))

      And("a fixed vector")
      val u = DenseVector(3.0, 4.0, 4.0, 3.0)

      When("asked to reschedule 4 times")
      val result = rescheduleTimes(times = 4, m, u, metric).head

      Then("the new matrix contributes in minimizing overall distance function")
      val betterCompatibility = metric(Types.synthesizeValues(result.matrix) + u)

      betterCompatibility should equal (metric.Lowest)

      And("the new vector should be equal to the only possible solution")
      result.matrix shouldEqual DenseMatrix((3.0, 0.0, 3.0, 0.0), (4.0, 0.0, 4.0, 0.0))
    }*/

  }
}
