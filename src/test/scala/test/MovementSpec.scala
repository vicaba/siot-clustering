package test

import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest._
import collection.CollecctionHelper._
import metrics.Metric
import org.scalatest.Matchers._

class MovementSpec extends FeatureSpec with GivenWhenThen {

  feature("Movement.biasedPeak reduces peak") {

    Given("a Movement")

    val flexibleLoad = SpanSlotFlexibleLoad(1, 0, Vector[Double](1, 1))

    val fixedLoad = SpanSlotFixedLoad(2, 0, Vector[Double](1, 1, 8))

    val acc = SpanSlotAccumulatedLoad(1, Set(fixedLoad, flexibleLoad))

    val mov = new Movement(acc, flexibleLoad, List(0, 1))

    When("Movement.biasedPeak is called")

    val result = mov.biasedPeak

    Then("it should return peak - 20%")

    result shouldBe (acc.peak * 0.2)



  }

}
