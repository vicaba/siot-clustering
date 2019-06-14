package test.reschedulermetrics
import test.{Movement, Rescheduler}

class BiasedAverageDistanceTransformation(val bias: Double = 0.50) extends MetricTransformation {

  override def apply(referenceAverage: Double, bestMovement: Movement, temporaryMovement: Movement, preferredSlots: List[Int]): (Double, Double) =
    (Rescheduler.computeAverageDistanceMetric(referenceAverage, temporaryMovement, preferredSlots),
      Rescheduler.computeAverageDistanceMetric(referenceAverage, bestMovement, preferredSlots))

}
