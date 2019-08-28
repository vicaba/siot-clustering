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

    def _reschedule(_acc: (AccumulatedLoad, AccumulatedLoad), _remainingFlexibleLoads: (List[FlexibleLoad], List[FlexibleLoad])): AccumulatedLoad =
      _remainingFlexibleLoads match {
        case (x :: xs, y :: ys) =>
          val newAcc = (_acc._1 += x, _acc._2 += y)
          rescheduleFlexibleLoad(newAcc, (x, y), preferredSlots, metricTransformation, referenceAverage, verbose)
          y.positionInT = x.positionInT
          _reschedule(newAcc, (xs, ys))
        case (Nil, Nil) => _acc._1
      }

    case class AccumulatedLoadWithSeparatedFlexibleLoads(acc: AccumulatedLoad, flexibleLoads: List[FlexibleLoad])

    def prepareAccumulatedLoadForAlgorithm(): (AccumulatedLoadWithSeparatedFlexibleLoads, AccumulatedLoadWithSeparatedFlexibleLoads) = {
      val bestAccumulatedLoad = acc.copy()
      val temporaryAccumulatedLoad = acc.copy()

      def splitFlexibleLoads(_acc: AccumulatedLoad): AccumulatedLoadWithSeparatedFlexibleLoads = {
        val remainingLoadsAfterRemovingFlexibleLoads = _acc.loads -- _acc.flexibleLoads
        val copy0 = _acc.copy()
        val copy = _acc.copy(loads = remainingLoadsAfterRemovingFlexibleLoads, copyFlexibleLoadSubtasks = false)
        AccumulatedLoadWithSeparatedFlexibleLoads(copy, _acc.flexibleLoads.toList.sorted(ordering))
      }

      (splitFlexibleLoads(bestAccumulatedLoad), splitFlexibleLoads(temporaryAccumulatedLoad))

    }

    val remainingLoadsAfterRemovingFlexibleLoads = acc.loads -- acc.flexibleLoads

    if (remainingLoadsAfterRemovingFlexibleLoads.nonEmpty) {
      val (best, temporary) = prepareAccumulatedLoadForAlgorithm()
      _reschedule((best.acc, temporary.acc), (best.flexibleLoads, temporary.flexibleLoads))
    } else
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
  def rescheduleFlexibleLoad(accumulatedLoad: (AccumulatedLoad, AccumulatedLoad),
    flexibleLoad: (FlexibleLoad, FlexibleLoad),
    preferredSlots: List[Int] = Nil,
    metricTransformation: MetricTransformation,
    referenceAverage: Double = 0.0,
    verbose: Boolean = false): AccumulatedLoad = {

    val bestAccumulatedLoad: AccumulatedLoad = accumulatedLoad._1
    val bestFlexibleLoad: FlexibleLoad = flexibleLoad._1

    val temporaryAccumulatedLoad: AccumulatedLoad = accumulatedLoad._2
    val temporaryFlexibleLoad: FlexibleLoad = flexibleLoad._2

    val bestMovement: Movement = new Movement(bestAccumulatedLoad, bestFlexibleLoad, preferredSlots)
    val temporaryMovement = new Movement(temporaryAccumulatedLoad, temporaryFlexibleLoad, preferredSlots)

    def moveTemporaryFlexibleLoadPositionInT(to: Int): Unit =
      temporaryFlexibleLoad.positionInT = to

    def updateBestMovement(withPositionInTForFlexibleLoad: Int): Unit =
      bestMovement.fl.positionInT = withPositionInTForFlexibleLoad

    def updateTemporaryMovement(withPositionInTForFlexibleLoad: Int): Unit =
      temporaryMovement.fl.positionInT = withPositionInTForFlexibleLoad


    if (verbose) println(s"Trying load ${bestFlexibleLoad.id}, load vector = ${bestFlexibleLoad.amplitudePerSlot.toString()}")
    //if (verbose) println(s"i -> ${accumulatedLoad.positionInT} until ${(accumulatedLoad.span - flexibleLoad.span) + 1}")
    for (i <- bestAccumulatedLoad.positionInT until ((bestAccumulatedLoad.span - bestFlexibleLoad.span) + 1)) {
      if (verbose) println(s"\tAt position $i")


      def move(): Unit = {
        updateTemporaryMovement(withPositionInTForFlexibleLoad = i)

        val metricResult =
          metricTransformation(referenceAverage, bestMovement, temporaryMovement, preferredSlots)

        val temporaryMetric = metricResult.temporaryMovementMetric
        val bestMetric      = metricResult.bestMovementMetric

        if (verbose) println(s"\t\tbestMetric = $bestMetric, peak = ${bestMovement.acc.peak}")
        if (verbose) print(s"\t\ttempMetric = $temporaryMetric, peak = ${temporaryMovement.acc.peak}")

        if (temporaryMetric < bestMetric) {
          if (verbose) println(" - Is best")

          updateBestMovement(withPositionInTForFlexibleLoad = temporaryFlexibleLoad.positionInT)
        } else {
          if (verbose) println(" - Not best")
        }

      }

      moveTemporaryFlexibleLoadPositionInT(to = i)

      temporaryFlexibleLoad match {
        case flst: FlexibleLoadSubTask if !flst.superTask.areAggregateesOverlapped =>
          move()
        case _: FlexibleLoadSubTask =>
          Unit
        case _: FlexibleLoad => move()
      }

    }

    bestAccumulatedLoad
  }

  def isLoadOnPreferredSlots(load: Load, preferedSlots: List[Int]): Boolean = {
    val flRange = for (i <- load.positionInT until (load.positionInT + load.span)) yield i

    flRange.forall(preferedSlots.contains(_))
  }

}
