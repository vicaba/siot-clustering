package algorithm.scheduler

import test.SpanSlotAccumulatedLoad
import test.reschedulermetrics.MetricTransformation

object Scheduler {

  def apply(acc: SpanSlotAccumulatedLoad,
    preferredSlots: List[Int] = Nil,
    metricTransformation: MetricTransformation,
    referenceAverage: Double = 0.0,
    verbose: Boolean = false): SpanSlotAccumulatedLoad =
    test.Rescheduler.reschedule(acc, preferredSlots, metricTransformation, referenceAverage, verbose)

}
