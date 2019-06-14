package test.reschedulermetrics

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import test.{Movement, RescheduleType, Rescheduler}

class AverageDistanceTransformationSpec extends FlatSpec {

  def metricBeforeRefactor(referenceAverage: Double, bestMovement: Movement, temporaryMovement: Movement, preferredSlots: List[Int]): (Double, Double) = (
  Rescheduler.computeAverageDistanceMetric2(referenceAverage, temporaryMovement, preferredSlots),
    Rescheduler.computeAverageDistanceMetric2(referenceAverage, bestMovement, preferredSlots)
  )

  val p = TransformationHelper.parameters

  metricBeforeRefactor(p.referenceAverage, p.mov, p.mov, p.preferredSlots) shouldBe
    AverageDistanceTransformation(p.referenceAverage, p.mov, p.mov, p.preferredSlots)
}
