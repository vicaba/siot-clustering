package test.reschedulermetrics

import test.Movement

trait MetricTransformation extends ((Double, Movement, Movement, List[Int]) => MetricTransformationResult) {
  override def apply(referenceAverage: Double,
                     bestMovement: Movement,
                     temporaryMovement: Movement,
                     preferredSlots: List[Int]): MetricTransformationResult
}

case class MetricTransformationResult(bestMovementMetric: Double, temporaryMovementMetric: Double)