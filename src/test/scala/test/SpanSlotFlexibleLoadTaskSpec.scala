package test

import algebra.SeqOps
import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._

class SpanSlotFlexibleLoadTaskSpec extends FlatSpec with GivenWhenThen {

  val subTask1 = Vector(2.0, 3.0, 4.0, 2.0)

  val subTask2 = Vector(2.0, 2.0)

  val lowValue = 1.0

  val spanSlotFlexibleLoad = SpanSlotFlexibleLoad(
    0,
    0,
    Vector.fill(3)(lowValue) ++ subTask1 ++ Vector.fill(4)(lowValue) ++ subTask2 ++ Vector(lowValue))

  "A SpanSlotFlexibleLoad" should "be correctly split and rebuilt" in {

    Given("a SpanSlotFlexibleLoad with multiple tasks")

    val fLoad = spanSlotFlexibleLoad

    When("transforming it into SpanSlotFlexibleLoadSuperTask (with subtasks)")

    val spanSlotFlexibleLoadSuperTask = SpanSlotFlexibleLoadTask.splitIntoSubTasks(
      fLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount)

    Then("it should contain 2 subtasks")

    spanSlotFlexibleLoadSuperTask.agregatees.length shouldBe 2

    And("they should preserve the values of the SpanSlotFlexibleLoad")

    spanSlotFlexibleLoadSuperTask.agregatees.map(_.amplitudePerSlot) should contain allOf (Vector(2.0, 3.0, 4.0, 2.0), Vector(
      2.0,
      2.0))

    And("it should be possible to rebuilt the original SpanSlotFlexibleLoad from the SpanSlotFlexibleLoadSuperTask")

    spanSlotFlexibleLoadSuperTask.toSpanSlotFlexibleLoad.amplitudePerSlot shouldBe fLoad.amplitudePerSlot

  }

  "Subtasks pertaining to a SpanSlotFlexibleLoadSuperTask, when rescheduled, changes" should "be reflected on the SpanSlotFlexibleLoadSuperTask" in {

    Given("a SpanSlotFlexibleLoadSuperTask")

    val spanSlotFlexibleLoadSuperTask = SpanSlotFlexibleLoadTask.splitIntoSubTasks(
      spanSlotFlexibleLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount)

    When("getting its subtasks")

    val subtasks = spanSlotFlexibleLoadSuperTask.agregatees

    And("rescheduling them at positionInT == 0")

    subtasks.foreach(_.positionInT = 0)

    Then("the SpanSlotFlexibleLoadSuperTask.amplitudePerSlot should be modified as expected")

    val sum = Vector(4.0, 5.0, 4.0, 2.0) ++ Vector.fill(spanSlotFlexibleLoadSuperTask.amplitudePerSlot.size - 4)(spanSlotFlexibleLoadSuperTask.restValue)

    spanSlotFlexibleLoadSuperTask.amplitudePerSlot shouldBe sum

  }

}
