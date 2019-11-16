package algorithm.scheduler

import metrics.Metric
import scheduler_model.load._
import Load._
import com.typesafe.scalalogging.Logger
import scheduler_model.scheduler.SchedulerAlgorithm
import scheduler_model.scheduler.metric_transformer.MetricTransformation
import scheduler_model.user_allocator.UserAllocator

import scala.util.Try

object Scheduler {

  val logger = Logger("Scheduler")

  def apply(clusters: List[AccumulatedLoad],
            metricTransformation: MetricTransformation,
            userOrdering: Ordering[FlexibleLoadRepresentation] = UserAllocator.DefaultOrdering,
            schedulerAlgorithmOrdering: Ordering[Load] = SchedulerAlgorithm.DefaultOrdering): List[AccumulatedLoad] = {

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

      logger.info(
        "Running Scheduler for {} users with user allocator {} and scheduler orderings {}",
        _clusters.length,
        userOrdering.toString,
        schedulerAlgorithmOrdering.toString
      )

      val numberOfSlots = LoadOps.span(_clusters)
      val schedulerPreferredSlots =
        UserAllocator.allocate(users = _clusters, userOrdering)

      val referenceAverage = _clusters.map(_.totalEnergy).sum / numberOfSlots / clusters.size

      val res = _clusters.zip(schedulerPreferredSlots).par.map {
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
      res.toList

    }

  }

  def apply(clusters: List[AccumulatedLoad],
            metricTransformation: MetricTransformation,
            userOrderings: List[Ordering[FlexibleLoadRepresentation]],
            schedulerAlgorithmOrderings: List[Ordering[Load]]): List[AccumulatedLoad] = {

    if (clusters.size == 1) {
      val orderings = SchedulerAlgorithm.DefaultOrderings

      var best: List[AccumulatedLoad] =
        apply(clusters, metricTransformation, UserAllocator.DefaultOrdering, orderings.head)
      var bestMetric = Metric.par(best)

      for (ordering <- orderings.tail) {
        val res       = apply(clusters, metricTransformation, UserAllocator.DefaultOrdering, ordering)
        val resMetric = Metric.par(res)
        if (resMetric < bestMetric) {
          best = res
          bestMetric = resMetric
        }
      }
      return best
    }

    val orderings = for {
      userOrdering               <- userOrderings
      schedulerAlgorithmOrdering <- schedulerAlgorithmOrderings
    } yield {
      (userOrdering, schedulerAlgorithmOrdering)
    }

    var best: List[AccumulatedLoad] = apply(clusters, metricTransformation, orderings.head._1, orderings.head._2)
    var bestMetric                  = Metric.par(best)

    for (ordering <- orderings.tail) {
      val res       = apply(clusters, metricTransformation, ordering._1, ordering._2)
      val resMetric = Metric.par(res)
      if (resMetric < bestMetric) {
        best = res
        bestMetric = resMetric
      }
    }
    best
  }

}
