package algorithm.scheduler

import metrics.Metric
import test.load.{AccumulatedLoad, Load}
import test.{SchedulerAlgorithm, UserAllocator}
import test.reschedulermetrics.MetricTransformation
import Load._

import scala.util.Try

object Scheduler {

  def apply(clusters: List[AccumulatedLoad],
            metricTransformation: MetricTransformation,
            userOrderings: List[Ordering[AccumulatedLoad]] = UserAllocator.DefaultOrderings,
            schedulerAlgorithmOrderings: List[Ordering[Load]] = SchedulerAlgorithm.DefaultOrderings)
    : List[AccumulatedLoad] = {

    (for {
      userOrdering               <- userOrderings
      schedulerAlgorithmOrdering <- schedulerAlgorithmOrderings
    } yield {

      val _clusters: List[AccumulatedLoad] = Load.deepCopy(clusters).toList

      val numberOfSlots    = AccumulatedLoad(-1, 0, _clusters, "").span
      val allFlexibleLoads = _clusters.flatMap(_.flexibleLoads)
      val windowSize       = Try(allFlexibleLoads.map(_.span).sum / allFlexibleLoads.size).getOrElse(1)
      val schedulerPreferredSlots =
        UserAllocator.allocate(users = _clusters, numberOfSlots = numberOfSlots, windowSize = windowSize, userOrdering)

      val referenceAverage = _clusters.map(_.totalEnergy).sum / numberOfSlots / clusters.size

      val res = _clusters.zip(schedulerPreferredSlots).map {
        case (user, schedulingPreferredSlotsForUser) =>
          SchedulerAlgorithm.reschedule(
            user,
            schedulingPreferredSlotsForUser,
            metricTransformation = metricTransformation,
            referenceAverage = referenceAverage,
            schedulerAlgorithmOrdering,
            verbose = false
          )
      }
      res

    }) minBy (Metric.par(_))
  }

}
