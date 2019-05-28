package test

import org.scalatest._
import test.Load._
import org.scalatest.Matchers._

class SpanSlotAccumulatedLoadSpec extends FeatureSpec with GivenWhenThen {

/*  feature("SpanSlotAccumulatedLoad accumulates fixed loads") {

    scenario("SpanSlotAccumulatedLoad accumulates one FixedLoad") {

      Given("one fixed load")

      val rawFixedLoads = Vector[Double](1.0)

      val fixedLoad = toFixedLoads(rawFixedLoads)

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, fixedLoad.toList, Set.empty)

      Then("amplitudePerSlot should contain a vector with a single element")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(rawFixedLoads)

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(rawFixedLoads.size)

    }

    scenario("SpanSlotAccumulatedLoad accumulates more than one FixedLoad") {

      Given("several fixed loads")

      val rawFixedLoads = Vector[Double](1.0, 2.0, 3.0, 4.0, 1.0, 34.0, 50.0, 100000.0)

      val fixedLoad = toFixedLoads(rawFixedLoads)

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, fixedLoad.toList, Set.empty)

      Then("amplitudePerSlot should contain a vector with multiple elements")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(rawFixedLoads)

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(rawFixedLoads.size)

    }

  }*/

  feature("SpanSlotAccumulatedLoad accumulates flexible loads") {

/*    scenario("SpanSlotAccumulatedLoad accumulates one FlexibleLoad") {

      Given("one flexible load")

      val rawFlexibleLoad = Vector[Double](1, 1, 1, 1)

      val flexibleLoad = SpanSlotFlexibleLoad(1, 0, rawFlexibleLoad)

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, List.empty, Set(flexibleLoad))

      Then("amplitudePerSlot should contain a vector with multiple elements")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(rawFlexibleLoad)

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(rawFlexibleLoad.size)

    }

    scenario("SpanSlotAccumulatedLoad accumulates more than one FlexibleLoad") {

      Given("several flexible loads")

      val rawFlexibleLoads = List(
        Vector[Double](1, 1, 1, 1),
        Vector[Double](1, 1, 4, 3)
      )

      val flexibleLoads = rawFlexibleLoads.zipWithIndex.map { case (l, idx) =>  SpanSlotFlexibleLoad(idx, idx, l) }

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, List.empty, flexibleLoads.toSet)

      Then("amplitudePerSlot should contain a vector with multiple elements")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(Vector[Double](1, 2, 2, 5, 3))

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(5)

    }*/

    scenario("SpanSlotAccumulatedLoad accumulates fixed and flexible loads") {

      Given("a fixed loads and a flexible load")

      val rawFixedLoads = Vector[Double](1.0, 2.0, 3.0, 4.0, 1.0, 34.0, 50.0, 100000.0)

      val fixedLoad = SpanSlotFixedLoad(0, 0, rawFixedLoads)

      val rawFlexibleLoads = List(
        Vector[Double](1, 1, 1, 1),
        Vector[Double](1, 1, 4, 3, 5)
      )

      val flexibleLoads = List(
        SpanSlotFlexibleLoad(1, 0, rawFlexibleLoads(0)),
        SpanSlotFlexibleLoad(2, 4, rawFlexibleLoads(1))
      )


      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, (fixedLoad :: flexibleLoads).toSet)

      Then("amplitudePerSlot should contain a vector with multiple elements")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(Vector[Double](2, 3, 4, 5, 2, 35, 54, 100003, 5))

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(rawFlexibleLoads.flatten.size)

    }

  }

}
