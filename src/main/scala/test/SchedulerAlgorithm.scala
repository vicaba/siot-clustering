package test

import test.reschedulermetrics.{BiasedAverageDistanceTransformation, BiasedPeakTransformation, MetricTransformation, NoTransformation}

import scala.annotation.tailrec

object SchedulerAlgorithm {

  def reschedule(acc: SpanSlotAccumulatedLoad,
                 preferredSlots: List[Int] = Nil,
                 metricTransformation: MetricTransformation,
                 referenceAverage: Double = 0.0,
                 verbose: Boolean = false): SpanSlotAccumulatedLoad = {

    @tailrec
    def _reschedule(_acc: SpanSlotAccumulatedLoad,
                    _remainingFlexibleLoads: List[SpanSlotFlexibleLoad]): SpanSlotAccumulatedLoad =
      _remainingFlexibleLoads match {
        case x :: xs =>
          _reschedule(rescheduleFlexibleLoad(_acc, x, preferredSlots, metricTransformation, referenceAverage, verbose), xs)
        case Nil => _acc
      }

    val remainingLoadsAfterRemovingFlexibleLoads = acc.loads -- acc.flexibleLoads

    if (remainingLoadsAfterRemovingFlexibleLoads.nonEmpty)
      _reschedule(acc.copy(loads = remainingLoadsAfterRemovingFlexibleLoads), acc.flexibleLoads.toList)
    else
      acc.copy()
  }

  // It deals with loads in sequence
  def rescheduleFlexibleLoad(accumulatedLoad: SpanSlotAccumulatedLoad,
                             flexibleLoad: SpanSlotFlexibleLoad,
                             preferredSlots: List[Int] = Nil,
                             metricTransformation: MetricTransformation,
                             referenceAverage: Double = 0.0,
                             verbose: Boolean = false): SpanSlotAccumulatedLoad = {

    // Used to perform mutable operations
    val temporaryX: SpanSlotAccumulatedLoad = accumulatedLoad.copy()

    var bestMovement: Movement = new Movement(accumulatedLoad += flexibleLoad, flexibleLoad, preferredSlots)
    if (verbose) println(s"Trying load ${flexibleLoad.id}, load vector = ${flexibleLoad.amplitudePerSlot.toString()}")
    //if (verbose) println(s"i -> ${accumulatedLoad.positionInT} until ${(accumulatedLoad.span - flexibleLoad.span) + 1}")
    for (i <- accumulatedLoad.positionInT until ((accumulatedLoad.span - flexibleLoad.span) + 1)) {
      if (verbose) println(s"\tAt position $i")

      val flexibleLoadMovement = flexibleLoad.copy(positionInT = i)
      val temporaryMovement =
        new Movement(temporaryX -/+= flexibleLoadMovement, flexibleLoadMovement, preferredSlots)

      val metricResult =
        metricTransformation(referenceAverage, bestMovement, temporaryMovement, preferredSlots)

      val temporaryMetric = metricResult.temporaryMovementMetric
      val bestMetric = metricResult.bestMovementMetric

      if (verbose) println(s"\t\tbestMetric = $bestMetric, peak = ${bestMovement.acc.peak}")
      if (verbose) print(s"\t\ttempMetric = $temporaryMetric, peak = ${temporaryMovement.acc.peak}")

       if (temporaryMetric < bestMetric) {
        if (verbose) println(" - Is best")

        bestMovement = new Movement(temporaryMovement.acc.copy(), temporaryMovement.fl, preferredSlots)
      } else {
        if (verbose) println(" - Not best")
      }
    }

    // TODO: Notice that we are returning a copy of the AccumulatedLoad, with a new mutable Set of Loads.
    accumulatedLoad -/+= flexibleLoad.positionInT_=(bestMovement.fl.positionInT)
    accumulatedLoad
  }

  def isLoadOnPreferredSlots(load: Load, preferedSlots: List[Int]): Boolean = {
    val flRange = for (i <- load.positionInT until (load.positionInT + load.span)) yield i

    flRange.forall(preferedSlots.contains(_))
  }

}
