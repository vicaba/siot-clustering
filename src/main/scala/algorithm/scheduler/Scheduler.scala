package algorithm.scheduler

import test.load.AccumulatedLoad
import test.{SchedulerAlgorithm, UserAllocator}
import test.reschedulermetrics.MetricTransformation

import scala.util.Try

object Scheduler {

  def apply(acc: AccumulatedLoad,
            preferredSlots: List[Int] = Nil,
            metricTransformation: MetricTransformation,
            referenceAverage: Double = 0.0,
            verbose: Boolean = false): AccumulatedLoad =
    test.SchedulerAlgorithm.reschedule(acc, preferredSlots, metricTransformation, referenceAverage, verbose)

  def apply(clusters: List[AccumulatedLoad],
            metricTransformation: MetricTransformation): List[AccumulatedLoad] = {

    val numberOfSlots    = AccumulatedLoad(-1, 0, clusters).span
    val allFlexibleLoads = clusters.flatMap(_.flexibleLoads)
    val windowSize       = Try(allFlexibleLoads.map(_.span).sum / allFlexibleLoads.size).getOrElse(1)
    val schedulerPreferredSlots =
      UserAllocator.allocate(users = clusters, numberOfSlots = numberOfSlots, windowSize = windowSize)

    val referenceAverage = clusters.map(_.totalEnergy).sum / numberOfSlots / clusters.size

    clusters.zip(schedulerPreferredSlots).map {
      case (user, schedulingPreferredSlotsForUser) =>
        SchedulerAlgorithm.reschedule(
          user,
          schedulingPreferredSlotsForUser,
          metricTransformation = metricTransformation,
          referenceAverage = referenceAverage, verbose = false
        )
    }

  }

}
