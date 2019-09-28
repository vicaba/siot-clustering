package scheduler_model.load.loadops

import breeze.linalg.DenseVector
import org.scalatest.{FlatSpec, GivenWhenThen}
import scheduler_model.load.{FixedLoad, FlexibleLoadSubTask, LoadOps}
import types.clusterer.DataTypeMetadata

class AggregatedAmplitudePerSlotSpec extends FlatSpec with GivenWhenThen {

  "Multiple loads" should "be correctly aggregated" in {

    implicit val amplitudePerSlotMetadata: DataTypeMetadata = DataTypeMetadata.generateDataTypeMetadata(forColumns = 4)

    Given("some loads")

    val fixedLoad = FixedLoad(0, 0, "FixedLoad", DenseVector[Double](0.0, 0.0, 0.1, 4.0))

    val flexibleLoadSubTask =
      FlexibleLoadSubTask(1, 0, "FlexibleLoadSubTask", startPositionInTime = 0, DenseVector[Double](5.0), null)

    val flexibleLoadSubTask2 =
      FlexibleLoadSubTask(2, 0, "FlexibleLoadSubTask2", startPositionInTime = 1, DenseVector[Double](5.0), null)

    val flexibleLoadSubTask3 =
      FlexibleLoadSubTask(3, 0, "FlexibleLoadSubTask3", startPositionInTime = 2, DenseVector[Double](5.0, 5.0), null)

    val loads = List(
      fixedLoad,
      flexibleLoadSubTask,
      flexibleLoadSubTask2,
      flexibleLoadSubTask3
    )

    When("aggregating their amplitudePerSlot")

    val aggregatedVector: DenseVector[Double] =
      LoadOps.aggregatedAmplitudePerSlot(loads, 0.0, amplitudePerSlotMetadata)

    Then("the result should be the expected vector")

    val expectedAggregatedVector: DenseVector[Double] = DenseVector[Double](5.0, 5.0, 5.1, 9.0)

    assert(
      expectedAggregatedVector == aggregatedVector,
      s"$aggregatedVector was not equal to expected $expectedAggregatedVector"
    )

  }


}
