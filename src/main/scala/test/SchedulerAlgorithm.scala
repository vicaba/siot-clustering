package test

import test.load.{AccumulatedLoad, FlexibleLoad, FlexibleLoadSubTask, Load}
import test.reschedulermetrics.{BiasedAverageDistanceTransformation, BiasedPeakTransformation, MetricTransformation, NoTransformation}

import scala.annotation.tailrec

object SchedulerAlgorithm {

  val DefaultOrdering: Ordering[Load] = Load.loadOrderingByAmplitude.reverse

  val DefaultOrderings: List[Ordering[Load]] = List(
    Load.loadOrderingByAmplitude,
    Load.loadOrderingByAmplitude.reverse,
    Load.loadOrderingByPositionInTime,
    Load.loadOrderingByPositionInTime.reverse
  )

  def reschedule(acc: AccumulatedLoad,
                 preferredSlots: List[Int] = Nil,
                 metricTransformation: MetricTransformation,
                 referenceAverage: Double = 0.0,
                 ordering: Ordering[Load] = DefaultOrdering,
                 verbose: Boolean = false): AccumulatedLoad = {

    @tailrec
    def _reschedule(_acc: AccumulatedLoad, _remainingFlexibleLoads: List[FlexibleLoad]): AccumulatedLoad =
      _remainingFlexibleLoads match {
        case x :: xs =>
          _reschedule(rescheduleFlexibleLoad(_acc, x, preferredSlots, metricTransformation, referenceAverage, verbose),
                      xs)
        case Nil => _acc
      }

    val remainingLoadsAfterRemovingFlexibleLoads = acc.loads -- acc.flexibleLoads

    if (remainingLoadsAfterRemovingFlexibleLoads.nonEmpty)
      _reschedule(acc.copy(loads = remainingLoadsAfterRemovingFlexibleLoads), acc.flexibleLoads.toList.sorted(ordering))
    else
      acc.copy()

  }

  /**
    * Mutates accumulatedLoad and flexibleLoad
    * @param accumulatedLoad
    * @param flexibleLoad
    * @param preferredSlots
    * @param metricTransformation
    * @param referenceAverage
    * @param verbose
    * @return
    */
  // It deals with loads in sequence
  def rescheduleFlexibleLoad(accumulatedLoad: AccumulatedLoad,
                             flexibleLoad: FlexibleLoad,
                             preferredSlots: List[Int] = Nil,
                             metricTransformation: MetricTransformation,
                             referenceAverage: Double = 0.0,
                             verbose: Boolean = false): AccumulatedLoad = {

    // Used to perform mutable operations
    val temporaryX: AccumulatedLoad = accumulatedLoad.copy()

    var bestMovement: Movement = new Movement(accumulatedLoad += flexibleLoad, flexibleLoad, preferredSlots)
    if (verbose) println(s"Trying load ${flexibleLoad.id}, load vector = ${flexibleLoad.amplitudePerSlot.toString()}")
    //if (verbose) println(s"i -> ${accumulatedLoad.positionInT} until ${(accumulatedLoad.span - flexibleLoad.span) + 1}")
    for (i <- accumulatedLoad.positionInT until ((accumulatedLoad.span - flexibleLoad.span) + 1)) {
      if (verbose) println(s"\tAt position $i")


      def move(flexibleLoadMovement: FlexibleLoad): Unit = {
        val temporaryMovement =
          new Movement(temporaryX -/+= flexibleLoadMovement, flexibleLoadMovement, preferredSlots)

        val metricResult =
          metricTransformation(referenceAverage, bestMovement, temporaryMovement, preferredSlots)

        val temporaryMetric = metricResult.temporaryMovementMetric
        val bestMetric      = metricResult.bestMovementMetric

        if (verbose) println(s"\t\tbestMetric = $bestMetric, peak = ${bestMovement.acc.peak}")
        if (verbose) print(s"\t\ttempMetric = $temporaryMetric, peak = ${temporaryMovement.acc.peak}")

        if (temporaryMetric < bestMetric) {
          if (verbose) println(" - Is best")

          bestMovement = new Movement(temporaryMovement.acc.copy(), temporaryMovement.fl, preferredSlots)
        } else {
          if (verbose) println(" - Not best")
        }

      }

      val _flexibleLoadMovement = flexibleLoad.copy(positionInT = i)

      _flexibleLoadMovement match {
        case flst: FlexibleLoadSubTask if !flst.superTask.areAggregateesOverlapped =>
          move(flst)
        case _: FlexibleLoadSubTask => Unit
        case fl: FlexibleLoad =>
          move(fl)
      }

    }

    accumulatedLoad -/+= flexibleLoad.positionInT_=(bestMovement.fl.positionInT)
    accumulatedLoad
  }

  def isLoadOnPreferredSlots(load: Load, preferedSlots: List[Int]): Boolean = {
    val flRange = for (i <- load.positionInT until (load.positionInT + load.span)) yield i

    flRange.forall(preferedSlots.contains(_))
  }

}
