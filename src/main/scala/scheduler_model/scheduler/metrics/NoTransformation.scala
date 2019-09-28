package scheduler_model.scheduler.metrics

import scheduler_model.scheduler.Movement

object NoTransformation extends MetricTransformation {

  override def apply(referenceAverage: Double,
    bestMovement: Movement,
    temporaryMovement: Movement,
    preferredSlots: List[Int]): MetricTransformationResult =
    MetricTransformationResult(bestMovement.acc.peak, temporaryMovement.acc.peak)

  override def toString(): String = "NoTransformation"

}
