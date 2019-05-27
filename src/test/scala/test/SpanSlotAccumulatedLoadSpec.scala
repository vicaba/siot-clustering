package test

import org.scalatest._
import test.Load._
import org.scalatest.Matchers._

class SpanSlotAccumulatedLoadSpec extends FeatureSpec with GivenWhenThen {

  feature("SpanSlotAccumulatedLoad accumulates fixed loads") {

    scenario("SpanSlotAccumulatedLoad accumulates one FixedLoad") {

      Given("one fixed load")

      val rawFixedLoads = Vector[Double](1.0)

      val fixedLoad = toFixedLoads(rawFixedLoads)

      When("creating a SpanSlotAccumulatedLoad")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, fixedLoad.toList, Set.empty)

      Then("amplitedePerSlot should contain a vector with a single element")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(rawFixedLoads)

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(rawFixedLoads.size)

    }

    scenario("SpanSlotAccumulatedLoad accumulates more than one FixedLoad") {

      Given("several fixed loads")

      val rawFixedLoads = Vector[Double](1.0, 2.0, 3.0, 4.0, 1.0, 34.0, 50.0, 100000.0)

      val fixedLoad = toFixedLoads(rawFixedLoads)

      When("creating a SpanSlotAccumulatedLoad")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, fixedLoad.toList, Set.empty)

      Then("amplitedePerSlot should contain a vector with multiple elements")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(rawFixedLoads)

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(rawFixedLoads.size)

    }


  }

}
