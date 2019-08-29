package algorithm.scheduler

import algorithm.Settings
import metrics.Metric
import test.{SchedulerAlgorithm, UserAllocator}
import test.load.{AccumulatedLoad, Load}
import test.reschedulermetrics.MetricTransformation

case class ReschedulerSettings(override val numberOfClusters: Int,
                               override val metric: Metric,
                               metricTransformation: MetricTransformation,
                               userOrderings: List[Ordering[AccumulatedLoad]] = UserAllocator.DefaultOrderings,
                               schedulerAlgorithmOrderings: List[Ordering[Load]] = SchedulerAlgorithm.DefaultOrderings)
    extends Settings
