package algorithm

import algorithm.scheduler.Scheduler
import metrics.Metric
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import reader.SyntheticProfilesReaderForScheduler
import test.load.{AccumulatedLoad, FixedLoad, FlexibleLoad, FlexibleLoadTask, Load}
import test.load.Load._
import test.SequenceSplitByConsecutiveElements
import test.reschedulermetrics.BiasedAverageDistanceTransformation

class SchedulerSpec extends FeatureSpec with GivenWhenThen {

  val MainFolder               = "files/syn_loads_test/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName   = "lighting_output.csv"

  feature("Scheduler. PAR is minimized after rescheduling") {

    scenario("With test data, both unscheduledLoads and scheduledLoads keep the same total energy") {

      Given("3 slots with 3 users and a flexible load each")

      val unscheduledLoads: List[AccumulatedLoad] = List(
        AccumulatedLoad(100,
                        0,
                        List(
                          FixedLoad(101, 0, Vector(2, 4, 1)),
                          FlexibleLoad(151, 0, Vector(3, 0, 0))
                        )),
        AccumulatedLoad(200,
                        0,
                        List(
                          FixedLoad(201, 0, Vector(2, 4, 1)),
                          FlexibleLoad(251, 0, Vector(3, 0, 0))
                        )),
        AccumulatedLoad(300,
                        0,
                        List(
                          FixedLoad(301, 0, Vector(2, 4, 1)),
                          FlexibleLoad(351, 0, Vector(3, 0, 0))
                        ))
      )

      unscheduledLoads.foreach(
        Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
          _,
          SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount))

      When("Scheduling loads")

      val copy = Load.deepCopy(unscheduledLoads).toList

      val scheduledLoads =
        Scheduler.apply(copy, new BiasedAverageDistanceTransformation)

      Then("ScheduledLoads PAR is lower than UnscheduledLoads PAR.")

      val unscheduledLoadsPar = Metric.par(unscheduledLoads)
      val scheduledLoadsPar   = Metric.par(scheduledLoads)

      scheduledLoadsPar should be < unscheduledLoadsPar

      info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
      info(s"PAR for scheduled loads: $scheduledLoadsPar.")

      And("scheduledLoads total energy is equal to unscheduledLoads total energy")

      scheduledLoads.map(_.totalEnergy).sum shouldBe unscheduledLoads.map(_.totalEnergy).sum

    }

    /*scenario("With synthetic data, PAR is minimized after rescheduling") {

      Given("Synthetically generated loads as UnscheduledLoads")

      val subFoldersAndIds: List[(String, Int)] = (for (i <- 2 to 3) yield (i + "/", i)).toList

      val unscheduledLoads = SyntheticProfilesReaderForScheduler
        .applyDefault(MainFolder,
          subFoldersAndIds.map(_._1),
          AppliancesOutputFileName,
          LightingOutputFileName,
          subFoldersAndIds.map(_._2),
          windowSize = 30)
        .toList

      unscheduledLoads.foreach(
        Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
          _,
          SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount))

      When("Scheduling loads")

      val scheduledLoads =
        Scheduler.apply(Load.deepCopy(unscheduledLoads).toList, new BiasedAverageDistanceTransformation)

      Then("ScheduledLoads PAR is lower than UnscheduledLoads PAR.")

      val unscheduledLoadsPar = Metric.par(unscheduledLoads)
      val scheduledLoadsPar = Metric.par(scheduledLoads)

      scheduledLoadsPar should be < unscheduledLoadsPar

      info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
      info(s"PAR for scheduled loads: $scheduledLoadsPar.")

      And("scheduledLoads total energy is equal to unscheduledLoads total energy")

      scheduledLoads.map(_.totalEnergy).sum shouldBe unscheduledLoads.map(_.totalEnergy).sum

    }*/

  }

}
