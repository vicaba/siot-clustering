package algorithm.scheduler

import metrics.Metric
import scheduler_model.load._
import Load._
import com.typesafe.scalalogging.Logger
import scheduler_model.scheduler.SchedulerAlgorithm
import scheduler_model.scheduler.metric_transformer.MetricTransformation
import scheduler_model.user_allocator.UserAllocator

import scala.util.Try

object SchedulerNewModel {

  val logger = Logger("Scheduler")


  def apply(clusters: List[AccumulatedLoad],
    metricTransformation: MetricTransformation,
    userOrdering: Ordering[AccumulatedLoad] = UserAllocator.DefaultOrdering,
    schedulerAlgorithmOrdering: Ordering[Load] = SchedulerAlgorithm.DefaultOrdering)
  : List[AccumulatedLoad] = {

    if (clusters.size == 1) {
      clusters.map { cluster =>
        logger.info("Running Scheduler for a single user")
        SchedulerAlgorithm.reschedule(
          cluster,
          Nil,
          metricTransformation = metricTransformation,
          referenceAverage = cluster.totalEnergy / cluster.span,
          schedulerAlgorithmOrdering,
          verbose = false
        )
      }

    } else {

      val _clusters: List[AccumulatedLoad] = LoadOps.copy(clusters).toList.asInstanceOf[List[AccumulatedLoad]]

      logger.info("Running Scheduler for {} users", _clusters.length)


      val numberOfSlots = LoadOps.span(_clusters)
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
