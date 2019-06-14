package test.reschedulermetrics

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import test.{RescheduleType, Rescheduler}

class BiasedAverageDistanceTransformationSpec extends FlatSpec {

  val p = TransformationHelper.parameters

  Rescheduler.computeMetrics(RescheduleType.MinimizeMeanDistance, p.referenceAverage, p.mov, p.mov, p.preferredSlots) shouldBe
    (new BiasedAverageDistanceTransformation)(p.referenceAverage, p.mov, p.mov, p.preferredSlots)


}
