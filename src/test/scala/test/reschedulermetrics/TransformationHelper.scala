package test.reschedulermetrics

import test.{Movement, SpanSlotAccumulatedLoad, SpanSlotFixedLoad, SpanSlotFlexibleLoad}

object TransformationHelper {

  class Parameters(val acc: SpanSlotAccumulatedLoad, val referenceAverage: Double, val mov: Movement, val preferredSlots: List[Int])

  val parameters: Parameters = {

    val flexibleLoad = SpanSlotFlexibleLoad(1, 1, Vector[Double](1, 1))

    val fixedLoad = SpanSlotFixedLoad(2, 0, Vector[Double](1, 1, 8, 8))

    val flexibleLoad2 = SpanSlotFlexibleLoad(3, 1, Vector[Double](2, 2))

    val acc = SpanSlotAccumulatedLoad(1, 0, Set(fixedLoad, flexibleLoad))

    val preferredSlots = List(2, 3)

    val mov = new Movement(acc, flexibleLoad, preferredSlots)

    val referenceAverage = {

      val acc2 = SpanSlotAccumulatedLoad(1, 0, Set(fixedLoad, flexibleLoad, flexibleLoad2))

      acc2.amplitudePerSlot.sum / acc2.span
    }

    new Parameters(acc, referenceAverage, mov, preferredSlots)

  }

}
