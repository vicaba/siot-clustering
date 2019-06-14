package test.reschedulermetrics
import test.{Movement, Rescheduler}

object AverageDistanceTransformation extends MetricTransformation {
  override def apply(referenceAverage: Double, bestMovement: Movement, temporaryMovement: Movement, preferredSlots: List[Int]): (Double, Double) =
    (Rescheduler.computeAverageDistanceMetric2(referenceAverage, temporaryMovement, preferredSlots),
      Rescheduler.computeAverageDistanceMetric2(referenceAverage, bestMovement, preferredSlots))
}
