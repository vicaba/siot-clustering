package test

class Movement(val acc: SpanSlotAccumulatedLoad, val fl: SpanSlotFlexibleLoad, preferredSlots: List[Int], bias: Double = 0.2)  {
  lazy val biasedPeak: Double = {
    val flRange = for (i <- fl.positionInT until (fl.positionInT + fl.span)) yield i
    if (flRange.forall(preferredSlots.contains(_))) {
      //acc.peak - (acc.peak * bias)
      val accMean = acc.totalEnergy / acc.span
      val newBias = 1 - accMean / acc.peak
      //println(s"\tTotalEnergy = ${acc.totalEnergy}, Span = ${acc.span}")
      //println(s"\tAccMean = $accMean, AccPeak = ${acc.peak}, Bias = $newBias")
      acc.peak - acc.peak * newBias
      acc.peak - (acc.peak * bias)
    }
    else acc.peak
  }
}