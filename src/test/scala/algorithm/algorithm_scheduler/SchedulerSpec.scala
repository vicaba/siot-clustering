package algorithm.algorithm_scheduler

import algorithm.scheduler.Scheduler
import metrics.Metric
import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._
import test.SequenceSplitByConsecutiveElements
import test.load._
import test.reschedulermetrics.BiasedAverageDistanceTransformation

class SchedulerSpec extends FlatSpec with GivenWhenThen {

  Given("A user with two loads")

  val unscheduledLoads: List[AccumulatedLoad] = List(
    AccumulatedLoad(100,
      0,
      List(
        FixedLoad(101, 0, Vector(4, 4, 4, 3, 3)),
        FlexibleLoad(151, 0, Vector(1, 1, 0, 0, 0))
      ))
  )

  unscheduledLoads.foreach(
    Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      _,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage))

  When("Scheduling loads")

  val copy = Load.deepCopy(unscheduledLoads).toList

  val scheduledLoads =
    Scheduler.apply(copy, new BiasedAverageDistanceTransformation)

  Then("ScheduledLoads PAR is lower or equal than UnscheduledLoads PAR.")

  val unscheduledLoadsPar = Metric.par(unscheduledLoads)
  val scheduledLoadsPar   = Metric.par(scheduledLoads)

  scheduledLoadsPar should be < unscheduledLoadsPar

  info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
  info(s"PAR for scheduled loads: $scheduledLoadsPar.")

  And("scheduledLoads total energy is equal to unscheduledLoads total energy")

  scheduledLoads.map(_.totalEnergy).sum shouldBe unscheduledLoads.map(_.totalEnergy).sum

}
