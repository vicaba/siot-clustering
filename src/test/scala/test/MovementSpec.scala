package test

import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest._
import collection.CollecctionHelper._
import metrics.Metric
import org.scalatest.Matchers._

class MovementSpec extends FeatureSpec with GivenWhenThen {

  feature("Movement.biasedPeak reduces peak if in range") {

    scenario("Movement.biasedPeak reduces peak when FlexibleLoad is in range") {

      Given("a Movement")

      val flexibleLoad = SpanSlotFlexibleLoad(1, 0, Vector[Double](1, 1))

      val fixedLoad = SpanSlotFixedLoad(2, 0, Vector[Double](1, 1, 8))

      val acc = SpanSlotAccumulatedLoad(1, 0, Set(fixedLoad, flexibleLoad))

      val mov = new Movement(acc, flexibleLoad, List(0, 1))

      When("Movement.biasedPeak is called")

      val result = mov.biasedPeak

      Then("it should return peak - 20%")

      result shouldEqual (acc.peak - (acc.peak * 0.2))

    }

    scenario("Movement.biasedPeak does not reduce peak when FlexibleLoad is out range") {

      Given("a Movement")

      val flexibleLoad = SpanSlotFlexibleLoad(1, 0, Vector[Double](1, 1))

      val fixedLoad = SpanSlotFixedLoad(2, 0, Vector[Double](1, 1, 8, 8))

      val acc = SpanSlotAccumulatedLoad(1, 0, Set(fixedLoad, flexibleLoad))

      val mov = new Movement(acc, flexibleLoad, List(2, 3))

      When("Movement.biasedPeak is called")

      val result = mov.biasedPeak

      Then("it should return peak")

      result shouldEqual acc.peak

    }

    scenario("Movement.biasedPeak does not reduce peak when FlexibleLoad is halfo out range") {

      Given("a Movement")

      val flexibleLoad = SpanSlotFlexibleLoad(1, 1, Vector[Double](1, 1))

      val fixedLoad = SpanSlotFixedLoad(2, 0, Vector[Double](1, 1, 8, 8))

      val acc = SpanSlotAccumulatedLoad(1, 0, Set(fixedLoad, flexibleLoad))

      val mov = new Movement(acc, flexibleLoad, List(2, 3))

      When("Movement.biasedPeak is called")

      val result = mov.biasedPeak

      Then("it should return peak")

      result shouldEqual acc.peak

    }


  }

}
