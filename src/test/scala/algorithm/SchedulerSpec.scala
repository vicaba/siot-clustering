package algorithm

import algebra.SeqOps
import algorithm.scheduler.Scheduler
import metrics.Metric
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import reader.SyntheticProfilesReaderForScheduler
import test.load.{AccumulatedLoad, FlexibleLoad, FlexibleLoadTask, Load}
import test.SequenceSplitByConsecutiveElements
import test.reschedulermetrics.BiasedAverageDistanceTransformation

class SchedulerSpec extends FeatureSpec with GivenWhenThen {

  val MainFolder               = "files/syn_loads_test/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName   = "lighting_output.csv"

  feature("Scheduler. PAR is minimized after rescheduling") {

    scenario("With synthetic data, PAR is minimized after rescheduling") {

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

      unscheduledLoads.foreach { accLoad =>
        val splitResult = accLoad.flexibleLoads.map { fLoad =>

          (fLoad, FlexibleLoadTask.splitIntoSubTasks(fLoad, SequenceSplitByConsecutiveElements
          .withConsecutiveValueAsTheHighestCount))

        }

        val flexibleLoadsToRemove = splitResult.map(_._1)
        val flexibleLoadsToAdd    = splitResult.flatMap { fLoadTask => List(fLoadTask._2.setComputeAmplitudePerSlotWithRestValueOnly(true)) ++ fLoadTask._2.aggregatees}

        accLoad --= flexibleLoadsToRemove
        accLoad ++= flexibleLoadsToAdd

      }

      When("Scheduling loads")

      val scheduledLoads = Scheduler.apply(Load.deepCopy(unscheduledLoads).toList, new BiasedAverageDistanceTransformation)

      Then("ScheduledLoads PAR is lower than UnscheduledLoads PAR.")


      val unscheduledLoadsPar = computePar(unscheduledLoads)
      val scheduledLoadsPar   = computePar(scheduledLoads)

      scheduledLoadsPar should be < unscheduledLoadsPar

      info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
      info(s"PAR for scheduled loads: $scheduledLoadsPar.")

      And("scheduledLoads total energy is equal to unscheduledLoads total energy")

      scheduledLoads.map(_.totalEnergy).sum shouldBe unscheduledLoads.map(_.totalEnergy).sum

    }

  }

  def computePar(loads: Iterable[Load]): Double = Metric.par(AccumulatedLoad(-1, 0, loads))
  def computePar(load: Load): Double            = Metric.par(AccumulatedLoad(-1, 0, load))

}
