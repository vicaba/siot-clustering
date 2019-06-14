package test.reschedulermetrics

import test.Movement

trait MetricTransformation extends ((Double, Movement, Movement, List[Int]) => (Double, Double)) {
  override def apply(referenceAverage: Double,
                     bestMovement: Movement,
                     temporaryMovement: Movement,
                     preferredSlots: List[Int]): (Double, Double)
}
