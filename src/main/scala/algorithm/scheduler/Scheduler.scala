package algorithm.scheduler

import metrics.Metric
import test.load.{AccumulatedLoad, Load}
import test.{SchedulerAlgorithm, SequenceSplitByConsecutiveElements, UserAllocator}
import test.reschedulermetrics.MetricTransformation
import Load._
import com.typesafe.scalalogging.Logger

import scala.util.Try

object Scheduler {

  val logger = Logger("Scheduler")


  def apply(clusters: List[AccumulatedLoad],
    metricTransformation: MetricTransformation,
    userOrdering: Ordering[AccumulatedLoad] = UserAllocator.DefaultOrderings.head,
    schedulerAlgorithmOrdering: Ordering[Load] = SchedulerAlgorithm.DefaultOrderings.head)
  : List[AccumulatedLoad] = {

    if (clusters.size == 1) {
      clusters.map {
        logger.info("Running Scheduler for a single user")
        SchedulerAlgorithm.reschedule(
          _,
          Nil,
          metricTransformation = metricTransformation,
          referenceAverage = 0.0,
          schedulerAlgorithmOrdering,
          verbose = false
        )
      }

    } else {

      val _clusters: List[AccumulatedLoad] = Load.deepCopy(clusters).toList

      logger.info("Running Scheduler for {} users", _clusters.length)


      val numberOfSlots = AccumulatedLoad(-1, 0, _clusters, "").span
      val allFlexibleLoads = _clusters.flatMap(_.flexibleLoads)
      val windowSize = Try(allFlexibleLoads.map(_.span).sum / allFlexibleLoads.size).getOrElse(1)
      val schedulerPreferredSlots =
        UserAllocator.allocate(users = _clusters, numberOfSlots = numberOfSlots, windowSize = windowSize, userOrdering)

      val referenceAverage = _clusters.map(_.totalEnergy).sum / numberOfSlots / clusters.size

      val res = _clusters.zip(schedulerPreferredSlots).map {
        case (user, schedulingPreferredSlotsForUser) =>
          logger.info("Running Scheduler for user {} with {} points", user.id, user.loads.size)
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

    }

  }

  def apply(clusters: List[AccumulatedLoad],
    metricTransformation: MetricTransformation,
    userOrderings: List[Ordering[AccumulatedLoad]],
    schedulerAlgorithmOrderings: List[Ordering[Load]])
  : List[AccumulatedLoad] = {

    (for {
      userOrdering <- userOrderings
      schedulerAlgorithmOrdering <- schedulerAlgorithmOrderings
    } yield {
      apply(clusters, metricTransformation, userOrdering, schedulerAlgorithmOrdering)
    }) minBy (Metric.par(_))
  }

}
