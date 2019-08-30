package test.reschedulermetrics

import test.Movement

class BiasedPeakTransformation(bias: Double = 0.2) extends MetricTransformation {

  override def apply(referenceAverage: Double,
                     bestMovement: Movement,
                     temporaryMovement: Movement,
                     preferredSlots: List[Int]): MetricTransformationResult =
    MetricTransformationResult(biasedPeak(bestMovement, preferredSlots), biasedPeak(temporaryMovement, preferredSlots))


  override def toString(): String = "BiasedPeakTransformation"

  private def biasedPeak(m: Movement, preferredSlots: List[Int]): Double = {

    val mPeak   = m.acc.peak
    val flRange = for (i <- m.fl.positionInT until (m.fl.positionInT + m.fl.span)) yield i

    if (flRange.forall(preferredSlots.contains(_))) {
      val accMean = m.acc.totalEnergy / m.acc.span
      val newBias = 1 - accMean / mPeak
      mPeak - (mPeak * bias)
    } else mPeak

  }

}
