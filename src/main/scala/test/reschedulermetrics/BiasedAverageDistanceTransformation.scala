package test.reschedulermetrics
import test.{Load, Movement, SpanSlotAccumulatedLoad}

class BiasedAverageDistanceTransformation(val bias: Double = 0.50) extends MetricTransformation {

  override def apply(referenceAverage: Double,
                     bestMovement: Movement,
                     temporaryMovement: Movement,
                     preferredSlots: List[Int]): MetricTransformationResult =
    MetricTransformationResult(
      computeAverageDistanceMetric(referenceAverage, bestMovement, preferredSlots, bias),
      computeAverageDistanceMetric(referenceAverage, temporaryMovement, preferredSlots, bias)
    )

  private def computeAverageDistanceMetric(referenceAverage: Double,
                                           movement: Movement,
                                           preferedSlots: List[Int],
                                           bias: Double = 0.50): Double = {
    val accLoad      = movement.acc
    val flexibleLoad = movement.fl

    val currentAverage                = computeAverageAtLoadPosition(accLoad, flexibleLoad)
    val slotsWithPriorityOverlapRatio = computeSlotsWithPriorityOverlapRatio(flexibleLoad, preferedSlots)

    val distance = Math.abs(referenceAverage - currentAverage)
    //println(s"average_ref = $referenceAverage, bias = $bias, slotsWithPriority = $slotsWithPriorityOverlapRatio")

    // TODO: It seems that removing bias works better
    distance * (1 - bias * slotsWithPriorityOverlapRatio)
  }

  private def computeSlotsWithPriorityOverlapRatio(load: Load, preferredSlots: List[Int]): Double =
    if (preferredSlots.isEmpty) 0.0
    else {
      val numberOfOverlappedSlots =
        (load.positionInT until (load.positionInT + load.span)).count(p => preferredSlots.contains(p))
      numberOfOverlappedSlots.toDouble / load.span.toDouble
    }

  private def computeAverageAtLoadPosition(accumulatedLoad: SpanSlotAccumulatedLoad, load: Load): Double =
    accumulatedLoad.amplitudePerSlot.slice(load.positionInT, load.positionInT + load.span).sum / load.span

}
