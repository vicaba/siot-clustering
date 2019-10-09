package scheduler_model.scheduler

import org.scalatest.Matchers._
import org.scalatest.{FeatureSpec, GivenWhenThen}
import scheduler_model.load.{AccumulatedLoad, Load, LoadOps}
import scheduler_model.reader.SyntheticProfilesReaderForScheduler2
import scheduler_model.scheduler.metric_transformer.BiasedAverageDistanceTransformation
import scheduler_model.sequence_split.SequenceSplitByConsecutiveElements
import types.clusterer.DataTypeMetadata
import metrics.Metric

class SchedulerAlgorithmSpec extends FeatureSpec with GivenWhenThen {

  feature("Rescheduler.rescheduleFlexibleLoad") {

    scenario("given two points clustered in one single cluster") {

      Given("Some points")

      val MainFolder = "files/syn_loads_test/"
      val AppliancesOutputFileName = "appliance_output.csv"
      val LightingOutputFileName = "lighting_output.csv"
      val subFoldersAndIds: List[(String, Int)] = (for (i <- 2 to 2) yield (i + "/", i)).toList

      val _unscheduledLoads = SyntheticProfilesReaderForScheduler2
        .applyDefault(MainFolder,
          subFoldersAndIds.map(_._1),
          AppliancesOutputFileName,
          LightingOutputFileName,
          subFoldersAndIds.map(_._2),
          windowSize = 30)

      val unscheduledLoad =
        AccumulatedLoad(1, 1, "cluster", _unscheduledLoads.foldLeft(Set.empty[Load]) {
          case (acc, loads) =>
            acc ++ loads.loads
        })(DataTypeMetadata.generateDataTypeMetadata(forColumns = 48))

      AccumulatedLoad.Mutate.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
        unscheduledLoad,
        SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

      When("Scheduling loads using SchedulerAlgorithm")

      val scheduledLoad =
        SchedulerAlgorithm.reschedule(LoadOps.copy(unscheduledLoad), metricTransformation = new BiasedAverageDistanceTransformation)

      Then("ScheduledLoads PAR is lower than UnscheduledLoads PAR.")

      val unscheduledLoadPar = Metric.par(unscheduledLoad)
      val scheduledLoadPar = Metric.par(scheduledLoad)

      scheduledLoadPar should be < unscheduledLoadPar

      info(s"PAR for unscheduled loads: $unscheduledLoadPar.")
      info(s"PAR for scheduled loads: $scheduledLoadPar.")

      And("scheduledLoads total energy is equal to unscheduledLoads total energy")

      scheduledLoad.totalEnergy shouldBe unscheduledLoad.totalEnergy

    }

  }

}
