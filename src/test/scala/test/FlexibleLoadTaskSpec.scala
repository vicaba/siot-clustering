package test

import algebra.SeqOps
import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._
import test.load.{AccumulatedLoad, FlexibleLoad, FlexibleLoadSubTask, FlexibleLoadTask, Load}

class FlexibleLoadTaskSpec extends FlatSpec with GivenWhenThen {

  val subTask1 = Vector(2.0, 3.0, 4.0, 2.0)

  val subTask2 = Vector(2.0, 2.0)

  val lowValue = 1.0

  val flexibleLoad =
    FlexibleLoad(0, 0, Vector.fill(3)(lowValue) ++ subTask1 ++ Vector.fill(4)(lowValue) ++ subTask2 ++ Vector(lowValue))

  "A SpanSlotFlexibleLoad" should "be correctly split and rebuilt" in {

    Given("a FlexibleLoad with multiple tasks")

    val fLoad = flexibleLoad

    When("transforming it into FlexibleLoadSuperTask (with subtasks)")

    val (flexibleLoadSuperTask, _) =
      FlexibleLoadTask.splitIntoSubTasks(fLoad,
                                         SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

    Then("it should contain 2 subtasks")

    flexibleLoadSuperTask.aggregatees.length shouldBe 2

    And("they should preserve the values of the SpanSlotFlexibleLoad")

    flexibleLoadSuperTask.aggregatees.map(_.amplitudePerSlot) should contain allOf (Vector(2.0, 3.0, 4.0, 2.0), Vector(
      2.0,
      2.0))

    And("it should be possible to rebuild the original FlexibleLoad from the FlexibleLoadSuperTask")

    flexibleLoadSuperTask.toSpanSlotFlexibleLoad.amplitudePerSlot shouldBe fLoad.amplitudePerSlot

  }

  "Subtasks pertaining to a FlexibleLoadSuperTask, when rescheduled, changes" should "be reflected on the FlexibleLoadSuperTask" in {

    Given("a SpanSlotFlexibleLoadSuperTask")

    val (spanSlotFlexibleLoadSuperTask, _) =
      FlexibleLoadTask.splitIntoSubTasks(flexibleLoad,
                                         SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

    When("getting its subtasks")

    val subtasks = spanSlotFlexibleLoadSuperTask.aggregatees

    And("rescheduling them at positionInT == 0")

    subtasks.foreach(_.positionInT = 0)

    Then("the SpanSlotFlexibleLoadSuperTask.amplitudePerSlot should be modified as expected")

    val sum = Vector(4.0, 5.0, 4.0, 2.0) ++ Vector.fill(spanSlotFlexibleLoadSuperTask.amplitudePerSlot.size - 4)(
      spanSlotFlexibleLoadSuperTask.restValue)

    spanSlotFlexibleLoadSuperTask.amplitudePerSlot shouldBe sum

  }

  "SuperTask and SubTasks" should "be copied down the hierarchy" in {

    Given("a supertask with two subtasks")

    val fLoad = flexibleLoad

    val superTask =
      FlexibleLoadTask.splitIntoSubTasks(fLoad,
                                         SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

    When("copy is performed on the super task")

    val superTaskCopy = superTask.copy()

    Then("object hash codes should be different")

    fail("This test does not have any assertion. Check periodically")

  }

  "AccumulatedLoad" should "be able to split into SubTasks and SuperTask and keep the same amplitudePerSlot" in {

    Given("an AccumulatedLoad with one FlexibleLoad")

    val accLoad = AccumulatedLoad(0, 0, List(flexibleLoad))

    val accLoadOriginal = accLoad.copy()

    When("getting the flexibleLoad and splitting it into subtasks")

    Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      accLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

    Then("accLoadOriginal amplitude per slot should be equal to accLoad total energy")

    accLoadOriginal.amplitudePerSlot shouldBe accLoad.amplitudePerSlot

    And("accLoadOriginal total energy should be equal to accLoad total energy")

    accLoadOriginal.totalEnergy shouldBe accLoad.totalEnergy

  }

  "Moving a subtask" should "not modify total energy" in {

    Given("an AccumulatedLoad with one FlexibleLoad")

    val accLoad = AccumulatedLoad(0, 0, List(flexibleLoad))

    val accLoadOriginal = accLoad.copy()

    When("getting the flexibleLoad and splitting it into subtasks")

    Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      accLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

    Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      accLoadOriginal,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

    And("moving a subtask does not modify totalEnergy")

    accLoad.flexibleLoads.head.positionInT = 10

    accLoad.totalEnergy shouldBe accLoadOriginal.totalEnergy

  }

  "A FlexibleLoadSuperTask with overlapped FlexibleLoadSubTasks" should "return that two subtasks are overlapped" in {

    Given("an AccumulatedLoad with one FlexibleLoad (with at least two subtasks) split into subtasks")

    val accLoad = AccumulatedLoad(0, 0, List(flexibleLoad))

    Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      accLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

    And("one subtask is overlapped with the other")

    accLoad.flexibleLoads.head.positionInT = 0
    accLoad.flexibleLoads.last.positionInT = 0

    When("checking if they are overlapped")

    val res = accLoad.flexibleLoads.head.asInstanceOf[FlexibleLoadSubTask].superTask.areAggregateesOverlapped

    Then("it should return true")

    res shouldBe true

  }

  "A FlexibleLoadSuperTask with no overlapped FlexibleLoadSubTasks" should "return that two subtasks are not overlapped" in {

    Given("an AccumulatedLoad with one FlexibleLoad (with at least two subtasks) split into subtasks")

    val accLoad = AccumulatedLoad(0, 0, List(flexibleLoad))

    Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      accLoad,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

    When("checking if they are overlapped")

    val res = accLoad.flexibleLoads.head.asInstanceOf[FlexibleLoadSubTask].superTask.areAggregateesOverlapped

    Then("it should return true")

    res shouldBe false

  }

}
