package test.reschedulermetrics

import test.Movement

object NoTransformation extends MetricTransformation {
  override def apply(referenceAverage: Double,
                     bestMovement: Movement,
                     temporaryMovement: Movement,
                     preferredSlots: List[Int]): (Double, Double) =
    (temporaryMovement.acc.peak, bestMovement.acc.peak)
}
