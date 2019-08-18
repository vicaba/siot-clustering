package test.reschedulermetrics
import test.load.{Load, AccumulatedLoad}
import test._

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
  }

  private def computeBiasedAverageAtLoadPosition(accumulatedLoad: AccumulatedLoad,
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
