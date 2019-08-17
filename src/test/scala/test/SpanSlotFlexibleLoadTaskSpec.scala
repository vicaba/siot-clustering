package test

import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._

class SpanSlotFlexibleLoadTaskSpec extends FlatSpec with GivenWhenThen {

  val subTask1 = Vector(2.0, 3.0, 4.0, 2.0)

  val subTask2 = Vector(2.0, 2.0)

  val lowValue = 1.0

  val spanSlotFlexibleLoad = SpanSlotFlexibleLoad(
    0,
    0,
    Vector.fill(3)(lowValue) ++ subTask1 ++ Vector.fill(4)(lowValue) ++ subTask2 ++ Vector(1.0))

  "A SpanSlotFlexibleLoad" should "be correctly split and rebuilt" in {

    Given("A SpanSlotFlexibleLoad with multiple tasks")

    val fLoad = spanSlotFlexibleLoad

    When("Transforming it into SpanSlotFlexibleLoadSuperTask (with subtasks)")

    val superTaskFlexibleLoad = SpanSlotFlexibleLoadTask.splitIntoSubTasks(
      fLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount)

    Then("it should contain 2 subtasks")

    superTaskFlexibleLoad.agregatees.length shouldBe 2

    And("they should preserve the values of the SpanSlotFlexibleLoad")

    superTaskFlexibleLoad.agregatees.map(_.amplitudePerSlot) should contain allOf (Vector(2.0, 3.0, 4.0, 2.0), Vector(
      2.0,
      2.0))

    And("It should be possible to rebuilt the original SpanSlotFlexibleLoad from the SpanSlotFlexibleLoadSuperTask")

    superTaskFlexibleLoad.toSpanSlotFlexibleLoad.amplitudePerSlot shouldBe fLoad.amplitudePerSlot

  }

}
