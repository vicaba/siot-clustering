package scheduler_model.scheduler.metrics

import breeze.linalg.sum
import scheduler_model.load.{AccumulatedLoad, Load}
import scheduler_model.scheduler.Movement

class BiasedAverageDistanceTransformation(val bias: Double = 0.50) extends MetricTransformation {

  override def apply(referenceAverage: Double,
    bestMovement: Movement,
    temporaryMovement: Movement,
    preferredSlots: List[Int]): MetricTransformationResult =
    MetricTransformationResult(
      computeAverageDistanceMetric(referenceAverage, bestMovement, preferredSlots, bias),
      computeAverageDistanceMetric(referenceAverage, temporaryMovement, preferredSlots, bias)
    )

  override def toString(): String = "BiasedAverageDistanceTransformation"

  private def computeAverageDistanceMetric(referenceAverage: Double,
    movement: Movement,
    preferredSlots: List[Int],
    bias: Double = 0.50): Double = {
    val accLoad      = movement.acc
    val flexibleLoad = movement.fl

    val currentAverage                = computeAverageAtLoadPosition(accLoad, flexibleLoad)
    val slotsWithPriorityOverlapRatio = computeSlotsWithPriorityOverlapRatio(flexibleLoad, preferredSlots)

    val distance = Math.abs(referenceAverage - currentAverage)
    //println(s"average_ref = $referenceAverage, bias = $bias, slotsWithPriority = $slotsWithPriorityOverlapRatio")

    // TODO: It seems that removing bias works better
    distance * (1 - slotsWithPriorityOverlapRatio)
  }

  private def computeSlotsWithPriorityOverlapRatio(load: Load, preferredSlots: List[Int]): Double =
    if (preferredSlots.isEmpty) 0.0
    else {
      val numberOfOverlappedSlots =
        (load.startPositionInTime until (load.startPositionInTime + load.span)).count(p => preferredSlots.contains(p))
      numberOfOverlappedSlots.toDouble / load.span.toDouble
    }

  private def computeAverageAtLoadPosition(accumulatedLoad: AccumulatedLoad, load: Load): Double =
    sum(accumulatedLoad.amplitudePerSlot.slice(load.startPositionInTime, load.startPositionInTime + load.span)) / load.span.toDouble

}