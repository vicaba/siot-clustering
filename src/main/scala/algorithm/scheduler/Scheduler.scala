package algorithm.scheduler

import test.{Rescheduler, SpanSlotAccumulatedLoad, UserAllocator}
import test.reschedulermetrics.MetricTransformation

import scala.util.Try

object Scheduler {

  def apply(acc: SpanSlotAccumulatedLoad,
            preferredSlots: List[Int] = Nil,
            metricTransformation: MetricTransformation,
            referenceAverage: Double = 0.0,
            verbose: Boolean = false): SpanSlotAccumulatedLoad =
    test.Rescheduler.reschedule(acc, preferredSlots, metricTransformation, referenceAverage, verbose)

  def apply(clusters: List[SpanSlotAccumulatedLoad],
            metricTransformation: MetricTransformation): List[SpanSlotAccumulatedLoad] = {

    val numberOfSlots    = SpanSlotAccumulatedLoad(-1, 0, clusters).span
    val allFlexibleLoads = clusters.flatMap(_.flexibleLoads)
    val windowSize       = Try(allFlexibleLoads.map(_.span).sum / allFlexibleLoads.size).getOrElse(1)
    val schedulerPreferredSlots =
      UserAllocator.allocate(users = clusters, numberOfSlots = numberOfSlots, windowSize = windowSize)

    val referenceAverage = clusters.map(_.totalEnergy).sum / numberOfSlots / clusters.size

    clusters.zip(schedulerPreferredSlots).map {
      case (user, schedulingPreferredSlotsForUser) =>
        Rescheduler.reschedule(
          user,
          schedulingPreferredSlotsForUser,
          metricTransformation = metricTransformation,
          referenceAverage = referenceAverage, verbose = true
        )
    }

  }

}
