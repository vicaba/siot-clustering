package algorithm.scheduler

import algorithm.Settings
import metrics.Metric
import scheduler_model.load.{AccumulatedLoad, FlexibleLoadRepresentation, Load}
import scheduler_model.scheduler.SchedulerAlgorithm
import scheduler_model.scheduler.metric_transformer.MetricTransformation
import scheduler_model.user_allocator.UserAllocator

case class ReschedulerSettings(override val numberOfClusters: Int,
                               override val metric: Metric,
                               metricTransformation: MetricTransformation,
                               userOrderings: List[Ordering[FlexibleLoadRepresentation]] = UserAllocator.DefaultOrderings,
                               schedulerAlgorithmOrderings: List[Ordering[Load]] = SchedulerAlgorithm.DefaultOrderings)
    extends Settings
