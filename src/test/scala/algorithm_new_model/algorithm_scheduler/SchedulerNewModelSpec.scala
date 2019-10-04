package algorithm_new_model.algorithm_scheduler

import algorithm.scheduler.SchedulerNewModel
import breeze.linalg.DenseVector
import metrics.Metric
import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._
import scheduler_model.load._
import scheduler_model.scheduler.metric_transformer.BiasedAverageDistanceTransformation
import scheduler_model.sequence_split.SequenceSplitByConsecutiveElements


class SchedulerNewModelSpec extends FlatSpec with GivenWhenThen {

  Given("A user with two loads")

  val unscheduledLoads: List[AccumulatedLoad] = List(
    AccumulatedLoad.AutoSpanFromLoads(100, 100, "100",
      List(
        FixedLoad(101, 101, "101", DenseVector(4, 4, 4, 3, 3)),
        FlexibleLoad(151, 151, "151", 0, DenseVector(1, 1, 0, 0, 0))
      ))
  )

  unscheduledLoads.foreach(
    AccumulatedLoad.Mutate.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      _,
      SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage))

  When("Scheduling loads")

  val copy = LoadOps.copy(unscheduledLoads).toList.asInstanceOf[List[AccumulatedLoad]]

  val scheduledLoads =
    SchedulerNewModel.apply(copy, new BiasedAverageDistanceTransformation)

  Then("ScheduledLoads PAR is lower or equal than UnscheduledLoads PAR.")

  val unscheduledLoadsPar = Metric.par(unscheduledLoads)
  val scheduledLoadsPar   = Metric.par(scheduledLoads)

  scheduledLoadsPar should be < unscheduledLoadsPar

  info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
  info(s"PAR for scheduled loads: $scheduledLoadsPar.")

  And("scheduledLoads total energy is equal to unscheduledLoads total energy")

  scheduledLoads.map(_.totalEnergy).sum shouldBe unscheduledLoads.map(_.totalEnergy).sum

}
