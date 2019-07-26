package test.reschedulermetrics

import test.Movement

object NoTransformation extends MetricTransformation {
  override def apply(referenceAverage: Double,
                     bestMovement: Movement,
                     temporaryMovement: Movement,
                     preferredSlots: List[Int]): MetricTransformationResult =
    MetricTransformationResult(bestMovement.acc.peak, temporaryMovement.acc.peak)
}
