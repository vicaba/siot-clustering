package scheduler_model.load

import breeze.linalg.DenseVector
import org.scalatest.{FlatSpec, GivenWhenThen}
import types.clusterer.DataTypeMetadata

class FlexibleLoadTaskSpec extends FlatSpec with GivenWhenThen {

  def GivenAFlexibleLoadSuperTaskWithThreeFlexibleLoadSubTasks(): FlexibleLoadSuperTask = {

    Given("three FlexibleLoadSubTasks")

    val flexibleLoadSubTask =
      FlexibleLoadSubTask(1, 0, "FlexibleLoadSubTask", startPositionInTime = 0, DenseVector[Double](4.0), null)

    val flexibleLoadSubTask2 =
      FlexibleLoadSubTask(2, 0, "FlexibleLoadSubTask2", startPositionInTime = 1, DenseVector[Double](5.0), null)

    val flexibleLoadSubTask3 =
      FlexibleLoadSubTask(3, 0, "FlexibleLoadSubTask3", startPositionInTime = 2, DenseVector[Double](5.0, 6.0), null)

    val flexibleLoads = List(
      flexibleLoadSubTask,
      flexibleLoadSubTask2,
      flexibleLoadSubTask3
    )

    And("a FlexibleLoadSuperTask aggregating the SubTasks")

    val flexibleLoadSuperTask =
      FlexibleLoadSuperTask(
        1,
        1,
        "FlexibleLoadSuperTask",
        amplitudeInOffStatus = 1.0,
        flexibleLoads, false)(DataTypeMetadata.generateDataTypeMetadata(6))

    flexibleLoadSuperTask
  }

  "A FlexibleLoadSubTask with three FlexibleLoadSubTasks" should "aggregate SubTasks correctly (with computeAmplitudePerSlotWithRestValueOnly to false)" in {

    val flexibleLoadSuperTask = GivenAFlexibleLoadSuperTaskWithThreeFlexibleLoadSubTasks()

    When("the FlexibleLoadSuperTask is asked for amplitudePerSlot")

    val amplitudePerSlot = flexibleLoadSuperTask.amplitudePerSlot

    Then("the result should be ethe expected vector")

    val expectedAmplitudePerSlot = DenseVector[Double](4.0, 5.0, 5.0, 6.0, 1.0, 1.0)

    assert(
      expectedAmplitudePerSlot == amplitudePerSlot,
      s"$amplitudePerSlot was not equal to expected $expectedAmplitudePerSlot"
    )

  }

  "A FlexibleLoadSubTask with three FlexibleLoadSubTasks" should "aggregate SubTasks correctly (with computeAmplitudePerSlotWithRestValueOnly to true)" in {

    val flexibleLoadSuperTask = GivenAFlexibleLoadSuperTaskWithThreeFlexibleLoadSubTasks()
    flexibleLoadSuperTask.computeAmplitudePerSlotWithRestValueOnly = true

    When("the FlexibleLoadSuperTask is asked for amplitudePerSlot")

    val amplitudePerSlot = flexibleLoadSuperTask.amplitudePerSlot

    Then("the result should be ethe expected vector")

    val expectedAmplitudePerSlot = DenseVector[Double](0.0, 0.0, 0.0, 0.0, 1.0, 1.0)

    assert(
      expectedAmplitudePerSlot == amplitudePerSlot,
      s"$amplitudePerSlot was not equal to expected $expectedAmplitudePerSlot"
    )

  }



}
