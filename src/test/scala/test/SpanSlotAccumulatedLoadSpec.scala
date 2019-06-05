package test

import algebra.SeqOps
import org.scalatest._
import test.Load._
import org.scalatest.Matchers._

import scala.collection.mutable

class SpanSlotAccumulatedLoadSpec extends FeatureSpec with GivenWhenThen {

  feature("SpanSlotAccumulatedLoad accumulates fixed loads") {

    scenario("SpanSlotAccumulatedLoad accumulates one FixedLoad") {

      Given("one fixed load")

      val rawFixedLoad = Vector[Double](1.0)

      val fixedLoad = SpanSlotFixedLoad(0, 0, rawFixedLoad)

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, fixedLoad)

      Then("amplitudePerSlot should contain a vector equal to the fixed load")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(rawFixedLoad)

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(rawFixedLoad.size)

    }

    scenario("SpanSlotAccumulatedLoad accumulates more than one FixedLoad") {

      Given("several fixed loads")


      val fixedLoads = List(
        SpanSlotFixedLoad(0, 0, Vector[Double](1, 2)),
        SpanSlotFixedLoad(1, 0, Vector[Double](1, 2)),
      )

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, fixedLoads)

      Then("amplitudePerSlot should contain a vector equal to the sum of rawFixedLoads")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(SeqOps.sum(fixedLoads.toList.map(_.amplitudePerSlot)))

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(fixedLoads.size)

    }

  }

  feature("SpanSlotAccumulatedLoad accumulates flexible loads") {

    scenario("SpanSlotAccumulatedLoad accumulates one FlexibleLoad") {

      Given("one flexible load")

      val rawFlexibleLoad = Vector[Double](1, 1, 1, 1)

      val flexibleLoad = SpanSlotFlexibleLoad(1, 0, rawFlexibleLoad)

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, flexibleLoad)

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

      val flexibleLoads = rawFlexibleLoads.zipWithIndex.map { case (l, idx) => SpanSlotFlexibleLoad(idx, idx, l) }

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, new mutable.HashSet[Load] ++= flexibleLoads)

      Then("amplitudePerSlot should contain a vector with multiple elements")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(Vector[Double](1, 2, 2, 5, 3))

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(5)

    }

    scenario("SpanSlotAccumulatedLoad accumulates fixed and flexible loads") {

      Given("a fixed loads and a flexible load")

      val rawFixedLoad = Vector[Double](1.0, 2.0, 3.0, 4.0, 1.0, 34.0, 50.0, 100000.0)

      val fixedLoad = SpanSlotFixedLoad(0, 0, rawFixedLoad)

      val rawFlexibleLoads = List(
        Vector[Double](1, 1, 1, 1),
        Vector[Double](1, 1, 4, 3, 5)
      )

      val flexibleLoads = List(
        SpanSlotFlexibleLoad(1, 0, rawFlexibleLoads(0)),
        SpanSlotFlexibleLoad(2, 4, rawFlexibleLoads(1))
      )

      When("a SpanSlotAccumulatedLoad is created")

      val spanSlotAccumulatedLoad = SpanSlotAccumulatedLoad(0, new mutable.HashSet[Load] ++= (fixedLoad :: flexibleLoads))

      Then("amplitudePerSlot should contain a vector with multiple elements")

      spanSlotAccumulatedLoad.amplitudePerSlot should equal(Vector[Double](2, 3, 4, 5, 2, 35, 54, 100003, 5))

      And("span should be equal to the number of fixed loads")

      spanSlotAccumulatedLoad.span should equal(rawFlexibleLoads.flatten.size)

    }

  }

}
