package test

import test.RescheduleType.RescheduleType
import test.reschedulermetrics.{BiasedAverageDistanceTransformation, BiasedPeakTransformation, MetricTransformation, NoTransformation}

import scala.annotation.tailrec

object Rescheduler {

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

  def rescheduleFlexibleLoad(accumulatedLoad: SpanSlotAccumulatedLoad,
                             flexibleLoad: SpanSlotFlexibleLoad,
                             preferredSlots: List[Int] = Nil,
                             metricTransformation: MetricTransformation,
                             referenceAverage: Double = 0.0,
                             verbose: Boolean = false): SpanSlotAccumulatedLoad = {

    // Used to perform mutable operations
    val temporaryX: SpanSlotAccumulatedLoad = accumulatedLoad.copy()

    def incrementInWindow(m: Movement): Double = {
      val slice = m.acc.amplitudePerSlot.slice(m.fl.positionInT, m.fl.positionInT + m.fl.span)
      slice.foldLeft(0.0) {
        case (acc, e) => acc + e * e
      } / slice.size
    }

    var bestMovement: Movement = new Movement(accumulatedLoad += flexibleLoad, flexibleLoad, preferredSlots)
    if (verbose) println(s"Trying load ${flexibleLoad.id}, load vector = ${flexibleLoad.amplitudePerSlot.toString()}")
    //if (verbose) println(s"i -> ${accumulatedLoad.positionInT} until ${(accumulatedLoad.span - flexibleLoad.span) + 1}")
    for (i <- accumulatedLoad.positionInT until ((accumulatedLoad.span - flexibleLoad.span) + 1)) {
      if (verbose) println(s"\tAt position $i")

      val flexibleLoadMovement = flexibleLoad.copy(positionInT = i)
      val temporaryNewMovement =
        new Movement(temporaryX -/+= flexibleLoadMovement, flexibleLoadMovement, preferredSlots)

      val (temporaryMetric, bestMetric) =
        metricTransformation(referenceAverage, bestMovement, temporaryNewMovement, preferredSlots)

      if (verbose) println(s"\t\tbestMetric = $bestMetric, peak = ${bestMovement.acc.peak}")
      if (verbose) print(s"\t\ttempMetric = $temporaryMetric, peak = ${temporaryNewMovement.acc.peak}")

       if (temporaryMetric < bestMetric) {
        if (verbose) println(" - Is best")

        bestMovement = new Movement(temporaryNewMovement.acc.copy(), temporaryNewMovement.fl, preferredSlots)
      } else {
        if (verbose) println(" - Not best")
      }
    }

    // TODO: Notice that we are returning a copy of the AccumulatedLoad, with a new mutable Set of Loads.
    bestMovement.acc

  }

  def isLoadOnPreferredSlots(load: Load, preferedSlots: List[Int]): Boolean = {
    val flRange = for (i <- load.positionInT until (load.positionInT + load.span)) yield i

    flRange.forall(preferedSlots.contains(_))
  }

}
