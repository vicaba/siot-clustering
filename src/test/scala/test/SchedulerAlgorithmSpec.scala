package test

import org.scalatest._
import collection.CollecctionHelper._
import metrics.Metric
import org.scalatest.Matchers._
import test.load.{AccumulatedLoad, FixedLoad, FlexibleLoad, FlexibleLoadSubTask, Load}
import test.reschedulermetrics.NoTransformation

class SchedulerAlgorithmSpec extends FeatureSpec with GivenWhenThen {

  feature("Rescheduler.rescheduleFlexibleLoad") {

    /*
    scenario("Rescheduler with only one flexible load") {

      Given("an SpanSlotAccumulatedLoad with only one flexible load")

      val rawFlexibleLoad = Vector[Double](1, 1, 1, 1)

      val flexibleLoad = FlexibleLoad(1, 0, rawFlexibleLoad)

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, flexibleLoad)

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.rescheduleFlexibleLoad(spanSlotAccumulatedLoad, flexibleLoad, metricTransformation = NoTransformation)

      Then("nothing should have happened")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(flexibleLoad.amplitudePerSlot)
    }

    scenario("Rescheduler with two flexible loads") {

      Given("an SpanSlotAccumulatedLoad with two loads")

      val rawFlexibleLoads = List(
        Vector[Double](1, 1, 1, 2),
        Vector[Double](2, 2, 2, 1)
      )

      val flexibleLoads = List(
        FlexibleLoad(1, 0, rawFlexibleLoads(0)),
        FlexibleLoad(2, 4, rawFlexibleLoads(1))
      )

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, flexibleLoads)

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.rescheduleFlexibleLoad(spanSlotAccumulatedLoad, flexibleLoads(1), metricTransformation = NoTransformation)

      Then("nothing should have happened")

      Metric.par(result) should be <= Metric.par(spanSlotAccumulatedLoad)

    }*/

  }

  feature("Rescheduler.reschedule") {

/*    scenario("Rescheduler with one flexible load") {

      Given("an SpanSlotAccumulatedLoad with only one flexible load")

      val rawFlexibleLoad = Vector[Double](1, 1, 1, 1)

      val flexibleLoad = FlexibleLoad(1, 0, rawFlexibleLoad)

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, flexibleLoad)

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.reschedule(spanSlotAccumulatedLoad, metricTransformation = NoTransformation)

      Then("nothing should have happened")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(flexibleLoad.amplitudePerSlot)
    }

    scenario("Rescheduler with one fixed load and one flexible load") {

      Given("an SpanSlotAccumulatedLoad with two loads")

      val loads = List(
        FlexibleLoad(1, 0, Vector[Double](1, 1)),
        FixedLoad(0, 0, Vector[Double](2, 1, 1, 2))
      )

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, loads)

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.reschedule(spanSlotAccumulatedLoad, metricTransformation = NoTransformation)

      Then("nothing should have happened")

      Metric.par(result) should be <= Metric.par(spanSlotAccumulatedLoad)

    }

    scenario("Rescheduler with two flexible loads and one fixed load as base") {

      Given("an SpanSlotAccumulatedLoad with three loads")

      val rawFlexibleLoads = List(
        Vector[Double](1, 1, 1, 2),
        Vector[Double](2, 2, 2, 1)
      )

      val loads = List(
        FlexibleLoad(1, 0, rawFlexibleLoads(0)),
        FlexibleLoad(2, 4, rawFlexibleLoads(1)),
        FixedLoad(3, 0, Vector[Double](1, 1, 1, 1, 1, 1, 1, 1))
      )

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, loads)

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.reschedule(spanSlotAccumulatedLoad, metricTransformation = NoTransformation)

      Then("nothing should have happened")

      Metric.par(result) shouldEqual Metric.par(spanSlotAccumulatedLoad)

    }*/

    scenario("a") {

        Given("an AccumulatedLoad with one FlexibleLoad (with at least two subtasks) split into subtasks")

        val subTask1 = Vector(2.0, 3.0, 4.0, 2.0)

        val subTask2 = Vector(2.0, 2.0)

        val lowValue = 1.0

        val flexibleLoad =
          FlexibleLoad(0, 0, Vector.fill(3)(lowValue) ++ subTask1 ++ Vector.fill(4)(lowValue) ++ subTask2 ++ Vector(lowValue))

        val accLoad = AccumulatedLoad(0, 0, List(flexibleLoad))

        val accLoadOriginal = accLoad.copy()

        Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
          accLoad,
          SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount)

        Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
          accLoadOriginal,
          SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount)

        And("one subtask is overlapped with the other")

        accLoad.flexibleLoads.head.positionInT = 0
        accLoad.flexibleLoads.last.positionInT = 0

        When("checking if they are overlapped")

        val res = SchedulerAlgorithm.reschedule(accLoad, metricTransformation = NoTransformation)

        Then("it should return true")

      }
    }
}
