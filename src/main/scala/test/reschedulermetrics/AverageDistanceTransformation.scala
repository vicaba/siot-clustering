package test.reschedulermetrics
import test.Rescheduler.computeBiasedAverageAtLoadPosition
import test.{Movement, Rescheduler}

object AverageDistanceTransformation extends MetricTransformation {
  override def apply(referenceAverage: Double, bestMovement: Movement, temporaryMovement: Movement, preferredSlots: List[Int]): (Double, Double) =
    (computeAverageDistanceMetric2(referenceAverage, temporaryMovement, preferredSlots),
      computeAverageDistanceMetric2(referenceAverage, bestMovement, preferredSlots))

  private def computeAverageDistanceMetric2(referenceAverage: Double,
                                    movement: Movement,
                                    preferedSlots: List[Int],
                                    bias: Double = 0.50): Double = {
    val actualAverage = computeBiasedAverageAtLoadPosition(movement.acc, movement.fl, preferedSlots, bias)

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

}
