package test.reschedulermetrics

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import test.{RescheduleType, Rescheduler}

class BiasedPeakTransFormationSpec extends FlatSpec {

  val p = TransformationHelper.parameters

  Rescheduler.computeMetrics(RescheduleType.BiasedPeak, p.referenceAverage, p.mov, p.mov, p.preferredSlots) shouldBe
    (new BiasedPeakTransformation)(p.referenceAverage, p.mov, p.mov, p.preferredSlots)

}
