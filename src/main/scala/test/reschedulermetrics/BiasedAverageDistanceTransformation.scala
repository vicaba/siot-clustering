package test.reschedulermetrics
import test.{Load, Movement, SpanSlotAccumulatedLoad}

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

    // TODO: It seems that removing bias works better
    distance * (1 - bias * slotsWithPriority)
  }

  private def computeSlotsWithPriority(load: Load, preferedSlots: List[Int]): Double = {
    val howManySlots = (load.positionInT until (load.positionInT + load.span)).count(p => preferedSlots.contains(p))
    //println(s"howManySlots = $howManySlots, size = ${preferedSlots.size}")

    if (preferedSlots.isEmpty) 0.0
    else howManySlots.toDouble / preferedSlots.size.toDouble
  }

  private def computeAverageAtLoadPosition(accumulatedLoad: SpanSlotAccumulatedLoad, load: Load): Double = {
    val fromSlot  = load.positionInT
    val untilSlot = load.positionInT + load.span

    val average = accumulatedLoad.amplitudePerSlot.slice(fromSlot, untilSlot).sum / load.span

    average
  }

}
