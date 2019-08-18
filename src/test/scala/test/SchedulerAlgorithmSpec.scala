package test

import org.scalatest._
import collection.CollecctionHelper._
import metrics.Metric
import org.scalatest.Matchers._
import test.load.{AccumulatedLoad, FixedLoad, FlexibleLoad}
import test.reschedulermetrics.NoTransformation

class SchedulerAlgorithmSpec extends FeatureSpec with GivenWhenThen {

  feature("Rescheduler.rescheduleFlexibleLoad") {

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

    }

  }

  feature("Rescheduler.reschedule") {

    scenario("Rescheduler with one flexible load") {

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

    }

  }
}
