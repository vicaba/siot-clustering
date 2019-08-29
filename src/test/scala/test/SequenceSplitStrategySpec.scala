package test

import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._

class SequenceSplitStrategySpec extends FlatSpec with GivenWhenThen {

  "A sequence with a larger number of consecutive values below average" should "be split into smaller sequences partitioned by the consecutive value" in {

    Given("a sequence with a larger number of consecutive values below average")

    val seq = Vector(1.0, 1.0, 1.0, 5.0, 5.0)

    When("splitting the sequence")

    val res = SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage(seq)

    Then("subsequences should not contain the lowest value")

    res.results.map(_.seq) should contain only Seq(5.0, 5.0)

  }

  "A sequence with a equal number of values below average than above average" should "be split into smaller sequences partitioned by the consecutive value" in {

    Given("a sequence with a larger number of consecutive values below average")

    val seq = Vector(1.0, 1.0, 5.0, 5.0)

    When("splitting the sequence")

    val res = SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage(seq)

    Then("subsequences should not contain the lowest value")

    res.results.map(_.seq) should contain only Seq(5.0, 5.0)

  }

  "A sequence with a lower number of values below average than above average" should "be split into smaller sequences partitioned by the consecutive value" in {

    Given("a sequence with a larger number of consecutive values below average")

    val seq = Vector(1.0, 5.0, 5.0)

    When("splitting the sequence")

    val res = SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage(seq)

    Then("subsequences should not contain the lowest value")

    res.results.map(_.seq) should contain only Seq(5.0, 5.0)

  }

}
