package test

import algebra.SeqOps
import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._
import test.load.{FlexibleLoad, FlexibleLoadTask}

class FlexibleLoadTaskSpec extends FlatSpec with GivenWhenThen {

  val subTask1 = Vector(2.0, 3.0, 4.0, 2.0)

  val subTask2 = Vector(2.0, 2.0)

  val lowValue = 1.0

  val spanSlotFlexibleLoad = FlexibleLoad(
    0,
    0,
    Vector.fill(3)(lowValue) ++ subTask1 ++ Vector.fill(4)(lowValue) ++ subTask2 ++ Vector(lowValue))

  "A SpanSlotFlexibleLoad" should "be correctly split and rebuilt" in {

    Given("a SpanSlotFlexibleLoad with multiple tasks")

    val fLoad = spanSlotFlexibleLoad

    When("transforming it into SpanSlotFlexibleLoadSuperTask (with subtasks)")

    val flexibleLoadSuperTask = FlexibleLoadTask.splitIntoSubTasks(
      fLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount)

    Then("it should contain 2 subtasks")

    flexibleLoadSuperTask.agregatees.length shouldBe 2

    And("they should preserve the values of the SpanSlotFlexibleLoad")

    flexibleLoadSuperTask.agregatees.map(_.amplitudePerSlot) should contain allOf (Vector(2.0, 3.0, 4.0, 2.0), Vector(
      2.0,
      2.0))

    And("it should be possible to rebuilt the original SpanSlotFlexibleLoad from the SpanSlotFlexibleLoadSuperTask")

    flexibleLoadSuperTask.toSpanSlotFlexibleLoad.amplitudePerSlot shouldBe fLoad.amplitudePerSlot

  }

  "Subtasks pertaining to a SpanSlotFlexibleLoadSuperTask, when rescheduled, changes" should "be reflected on the SpanSlotFlexibleLoadSuperTask" in {

    Given("a SpanSlotFlexibleLoadSuperTask")

    val spanSlotFlexibleLoadSuperTask = FlexibleLoadTask.splitIntoSubTasks(
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

  "Super task and subtasks" should "be copied down the hierarchy" in {

    Given("a supertask with two subtasks")

    val fLoad = spanSlotFlexibleLoad

    val superTask = FlexibleLoadTask.splitIntoSubTasks(
      fLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount)

    When("copy is performed on the super task")

    val superTaskCopy = superTask.copy()

    Then("object hash codes should be different")

  }

}
