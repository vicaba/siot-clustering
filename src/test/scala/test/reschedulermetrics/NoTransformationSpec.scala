package test.reschedulermetrics

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import test.{RescheduleType, Rescheduler}

class NoTransformationSpec extends FlatSpec {

  val p = TransformationHelper.parameters

  Rescheduler.computeMetrics(RescheduleType.MinimizePeak, p.referenceAverage, p.mov, p.mov, p.preferredSlots) shouldBe
    NoTransformation(p.referenceAverage, p.mov, p.mov, p.preferredSlots)


}
