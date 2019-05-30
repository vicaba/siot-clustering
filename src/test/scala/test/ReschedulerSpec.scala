package test

import org.scalatest._
import collection.CollecctionHelper._
import metrics.Metric
import org.scalatest.Matchers._

class ReschedulerSpec extends FeatureSpec with GivenWhenThen {

  feature("ReschedulerSpec") {

    scenario("Rescheduler with only one load") {

      Given("an SpanSlotAccumulatedLoad with only one flexible load")

      val rawFlexibleLoad = Vector[Double](1, 1, 1, 1)

      val flexibleLoad = SpanSlotFlexibleLoad(1, 0, rawFlexibleLoad)

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, flexibleLoad)

      When("the Rescheduler is called")

      val result = Rescheduler.reschedule(spanSlotAccumulatedLoad, flexibleLoad)

      Then("nothing should have happened")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(flexibleLoad.amplitudePerSlot)
    }

    scenario("Rescheduler with only two loads") {

      Given("an SpanSlotAccumulatedLoad with two loads")

      val rawFlexibleLoads = List(
        Vector[Double](1, 1, 1, 2),
        Vector[Double](2, 2, 2, 1)
      )

      val flexibleLoads = List(
        SpanSlotFlexibleLoad(1, 0, rawFlexibleLoads(0)),
        SpanSlotFlexibleLoad(2, 4, rawFlexibleLoads(1))
      )

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, flexibleLoads)

      When("the Rescheduler is called")

      val result = Rescheduler.reschedule(spanSlotAccumulatedLoad, flexibleLoads(1))

      Then("nothing should have happened")

      Metric.par(result) should be <= Metric.par(spanSlotAccumulatedLoad)

    }

  }
}
