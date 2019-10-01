/*
package scheduler_model.scheduler

import scheduler_model.load._
import scheduler_model.scheduler.metrics.MetricTransformation

object SchedulerAlgorithm {

  def reschedule(acc: AccumulatedLoad,
    preferredSlots: List[Int] = Nil,
    metricTransformation: MetricTransformation,
    referenceAverage: Double = 0.0,
    ordering: Ordering[Load] = DefaultOrdering,
    verbose: Boolean = false): AccumulatedLoad = {

    def _reschedule(_acc: (AccumulatedLoad, AccumulatedLoad), _remainingFlexibleLoads: (List[FlexibleLoad], List[FlexibleLoad])): AccumulatedLoad =
      _remainingFlexibleLoads match {
        case (x :: xs, y :: ys) =>
          //val newAcc = (_acc._1 += x, _acc._2 += y)
          //rescheduleFlexibleLoad(newAcc, (x, y), preferredSlots, metricTransformation, referenceAverage, verbose)
          //y.positionInT = x.positionInT
          //_reschedule(newAcc, (xs, ys))
        case (Nil, Nil) => _acc._1
      }

    case class AccumulatedLoadWithSeparatedFlexibleLoads(acc: AccumulatedLoad, flexibleLoads: List[FlexibleLoad])

    def prepareAccumulatedLoadForAlgorithm(): (AccumulatedLoadWithSeparatedFlexibleLoads, AccumulatedLoadWithSeparatedFlexibleLoads) = {
      val bestAccumulatedLoad = Load.deepCopy(List(acc)).head
      val temporaryAccumulatedLoad = Load.deepCopy(List(acc)).head

      def splitFlexibleLoads(_acc: AccumulatedLoad): AccumulatedLoadWithSeparatedFlexibleLoads = {
        val remainingLoadsAfterRemovingFlexibleLoads = _acc.loads -- _acc.flexibleLoads
        val copy = _acc.copy(copyFlexibleLoadSubtasks = false)
        // Those operations are done to mantain the references, so copy has the references of _acc
        copy --= _acc.loads
        copy ++= remainingLoadsAfterRemovingFlexibleLoads

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

}
*/
