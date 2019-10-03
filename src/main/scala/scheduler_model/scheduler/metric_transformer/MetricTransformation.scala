package scheduler_model.scheduler.metric_transformer

import scheduler_model.scheduler.Movement

trait MetricTransformation extends ((Double, Movement, Movement, List[Int]) => MetricTransformationResult) {
  override def apply(referenceAverage: Double,
    bestMovement: Movement,
    temporaryMovement: Movement,
    preferredSlots: List[Int]): MetricTransformationResult
}
