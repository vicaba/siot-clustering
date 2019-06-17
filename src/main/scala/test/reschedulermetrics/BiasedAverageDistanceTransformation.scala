package test.reschedulermetrics
import test.Rescheduler.{computeAverageAtLoadPosition, computeSlotsWithPriority}
import test.{Movement, Rescheduler}

class BiasedAverageDistanceTransformation(val bias: Double = 0.50) extends MetricTransformation {

  override def apply(referenceAverage: Double, bestMovement: Movement, temporaryMovement: Movement, preferredSlots: List[Int]): (Double, Double) =
    (computeAverageDistanceMetric(referenceAverage, temporaryMovement, preferredSlots, bias),
      computeAverageDistanceMetric(referenceAverage, bestMovement, preferredSlots, bias))

  private def computeAverageDistanceMetric(referenceAverage: Double,
                                   movement: Movement,
                                   preferedSlots: List[Int],
                                   bias: Double = 0.50): Double = {
    val acc          = movement.acc
    val flexibleLoad = movement.fl

    val actualAverage     = computeAverageAtLoadPosition(acc, flexibleLoad)
    val slotsWithPriority = computeSlotsWithPriority(flexibleLoad, preferedSlots)

    val distance = Math.pow(Math.abs(referenceAverage - actualAverage), 1)
    println(s"average_ref = $referenceAverage, bias = $bias, slotsWithPriority = $slotsWithPriority")

    distance * (1 - bias * slotsWithPriority)
  }

}
