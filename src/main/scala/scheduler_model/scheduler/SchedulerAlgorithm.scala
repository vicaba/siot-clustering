package scheduler_model.scheduler

import scheduler_model.load._
import scheduler_model.scheduler.metric_transformer.MetricTransformation

object SchedulerAlgorithm {

  val DefaultOrdering: Ordering[Load] = Load.loadOrderingByAmplitude.reverse

  val DefaultOrderings: List[Ordering[Load]] = List(
    Load.loadOrderingByAmplitude.reverse,
    Load.loadOrderingByAmplitude,
    Load.loadOrderingByPositionInTime.reverse,
    Load.loadOrderingByPositionInTime,
  )

  def reschedule(
      acc: AccumulatedLoad,
      preferredSlots: List[Int] = Nil,
      metricTransformation: MetricTransformation,
      referenceAverage: Double = 0.0,
      ordering: Ordering[Load] = DefaultOrdering,
      verbose: Boolean = false,
      flexibleLoadTransformer: (=> AccumulatedLoad, (FlexibleLoad, FlexibleLoad)) => (FlexibleLoad, FlexibleLoad) = (_, loads) => (loads._1, loads._2)): AccumulatedLoad = {

    def _reschedule(_acc: (AccumulatedLoad, AccumulatedLoad),
                    _remainingFlexibleLoads: (List[FlexibleLoad], List[FlexibleLoad])): AccumulatedLoad =
      _remainingFlexibleLoads match {
        case (x :: xs, y :: ys) =>
          val (transformedX, transformedY) = flexibleLoadTransformer(LoadOps.copy(_acc._1), (x, y))
          // TODO: Feedback -> Transform here
          val newAcc = (_acc._1 += transformedX, _acc._2 += transformedY)
          rescheduleFlexibleLoad(newAcc,
                                 (transformedX, transformedY),
                                 preferredSlots,
                                 metricTransformation,
                                 referenceAverage,
                                 verbose)
          transformedY.startPositionInTime = transformedX.startPositionInTime
          _reschedule(newAcc, (xs, ys))
        case (Nil, Nil) => _acc._1
      }

    case class AccumulatedLoadWithSeparatedFlexibleLoads(acc: AccumulatedLoad, flexibleLoads: List[FlexibleLoad])

    def prepareAccumulatedLoadForAlgorithm()
      : (AccumulatedLoadWithSeparatedFlexibleLoads, AccumulatedLoadWithSeparatedFlexibleLoads) = {
      val bestAccumulatedLoad      = LoadOps.copy(acc, addSuperTaskSubTasks = true)
      val temporaryAccumulatedLoad = LoadOps.copy(acc, addSuperTaskSubTasks = true)

      def splitFlexibleLoads(_acc: AccumulatedLoad): AccumulatedLoadWithSeparatedFlexibleLoads = {
        // Retrieve FlexibleLoads
        val flexibleLoads = _acc.flexibleLoads
        _acc.loads --= _acc.flexibleLoads
        AccumulatedLoadWithSeparatedFlexibleLoads(_acc, flexibleLoads.toList.sorted(ordering))
      }

      (splitFlexibleLoads(bestAccumulatedLoad), splitFlexibleLoads(temporaryAccumulatedLoad))

    }

    val remainingLoadsAfterRemovingFlexibleLoads = acc.loads -- acc.flexibleLoads

    if (remainingLoadsAfterRemovingFlexibleLoads.nonEmpty) {
      val (best, temporary) = prepareAccumulatedLoadForAlgorithm()
      //println("order")
      _reschedule((best.acc, temporary.acc), (best.flexibleLoads, temporary.flexibleLoads))
    } else
      LoadOps.copy(acc, addSuperTaskSubTasks = true)

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
    val bestFlexibleLoad: FlexibleLoad       = flexibleLoad._1

    val temporaryAccumulatedLoad: AccumulatedLoad = accumulatedLoad._2
    val temporaryFlexibleLoad: FlexibleLoad       = flexibleLoad._2

    val bestMovement: Movement = new Movement(bestAccumulatedLoad, bestFlexibleLoad, preferredSlots)
    val temporaryMovement      = new Movement(temporaryAccumulatedLoad, temporaryFlexibleLoad, preferredSlots)

    def moveTemporaryFlexibleLoadPositionInT(to: Int): Unit =
      temporaryFlexibleLoad.startPositionInTime = to

    def updateBestMovement(withPositionInTForFlexibleLoad: Int): Unit =
      bestMovement.fl.startPositionInTime = withPositionInTForFlexibleLoad

    def updateTemporaryMovement(withPositionInTForFlexibleLoad: Int): Unit =
      temporaryMovement.fl.startPositionInTime = withPositionInTForFlexibleLoad

    if (verbose)
      println(s"Trying load ${bestFlexibleLoad.id}, load vector = ${bestFlexibleLoad.amplitudePerSlot.toString()}")
    //if (verbose) println(s"i -> ${accumulatedLoad.positionInT} until ${(accumulatedLoad.span - flexibleLoad.span) + 1}")
    for (i <- bestAccumulatedLoad.startPositionInTime until ((bestAccumulatedLoad.span - bestFlexibleLoad.span) + 1)) {
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

          updateBestMovement(withPositionInTForFlexibleLoad = temporaryFlexibleLoad.startPositionInTime)
        } else {
          if (verbose) println(" - Not best")
        }

      }

      moveTemporaryFlexibleLoadPositionInT(to = i)

      temporaryFlexibleLoad match {
        case flst: FlexibleLoadSubTask if !flst.superTask.areAggregateesOverlapped =>
          move()
        case _: FlexibleLoadSubTask => Unit // If the subtask collides with another subtask,
        // do nothing, "i" will increment the next loop
        case _: FlexibleLoad => move()
      }

    }

    bestAccumulatedLoad
  }

}
