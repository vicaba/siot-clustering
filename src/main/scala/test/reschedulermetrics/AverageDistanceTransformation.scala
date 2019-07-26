package test.reschedulermetrics
import test.{Load, Movement, SpanSlotAccumulatedLoad}

object AverageDistanceTransformation extends MetricTransformation {
  override def apply(referenceAverage: Double,
                     bestMovement: Movement,
                     temporaryMovement: Movement,
                     preferredSlots: List[Int]): MetricTransformationResult =
    MetricTransformationResult(
      computeAverageDistanceMetric2(referenceAverage, bestMovement, preferredSlots),
      computeAverageDistanceMetric2(referenceAverage, temporaryMovement, preferredSlots)
    )

  private def computeAverageDistanceMetric2(referenceAverage: Double,
                                            movement: Movement,
                                            preferredSlots: List[Int],
                                            bias: Double = 0.50): Double = {
    val actualAverage = computeBiasedAverageAtLoadPosition(movement.acc, movement.fl, preferredSlots, bias)

    val distance = Math.pow(Math.abs(referenceAverage - actualAverage), 2)

    distance
    /*var distance = 0.0
    val p = 1

    val flexLoad = movement.fl

    for (i <- flexLoad.positionInT until (flexLoad.positionInT + flexLoad.span)) {
      distance += Math.pow(Math.abs(referenceAverage - movement.acc.amplitudePerSlot(i)), p) * (1 - bias)
    }

    Math.pow(distance, 1 / p) * (1 - bias)*/
  }

  private def computeBiasedAverageAtLoadPosition(accumulatedLoad: SpanSlotAccumulatedLoad,
                                                 load: Load,
                                                 preferredSlots: List[Int],
                                                 bias: Double): Double = {
    val fromSlot  = load.positionInT
    val untilSlot = load.positionInT + load.span

    var sum = 0.0
    for (i <- fromSlot until untilSlot) {
      val amplitude = accumulatedLoad.amplitudePerSlot(i)
      val biasedAmplitude = {
        if (preferredSlots.contains(i)) amplitude * bias
        else amplitude
      }
      sum += biasedAmplitude
    }

    val average = sum / load.span

    average
  }

}
