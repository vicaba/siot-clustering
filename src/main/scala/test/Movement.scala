package test

class Movement(val acc: SpanSlotAccumulatedLoad, val fl: SpanSlotFlexibleLoad, preferredSlots: List[Int])  {
  lazy val biasedPeak: Double = {
    val flRange = for (i <- fl.positionInT until (fl.positionInT + fl.span)) yield i
    if (flRange.forall(preferredSlots.contains(_))) acc.peak - (acc.peak * 0.2)
    else acc.peak
  }
}