package algorithm

import algorithm.scheduler.Scheduler
import metrics.Metric
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import reader.SyntheticProfilesReaderForScheduler
import test.load.{AccumulatedLoad, FlexibleLoad, FlexibleLoadTask, Load}
import test.SequenceSplitByConsecutiveElements
import test.reschedulermetrics.BiasedAverageDistanceTransformation

class SchedulerSpec2 extends FeatureSpec with GivenWhenThen {

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
          val splitResults =
            SequenceSplitByConsecutiveElements
              .withConsecutiveValueAsTheHighestCount(fLoad.amplitudePerSlot)
          val splitFlexibleLoad = splitResults.results.zipWithIndex
            .map {
              case (s, idx) =>
                FlexibleLoad(idx, s.index, s.seq.toVector, fLoad.label)
            }
          (fLoad, splitFlexibleLoad)
        }

        //TODO: This is because all flexible loads need to be removed, otherwise flexible loads that are OFF distort the Scheduler
        //TODO: Also, we should find a manner to expand flexible loads that have an OFF power greater than 0.0, it will distort metrics otherwise
        val onFlexibleLoads = splitResult
        //val workingFlexibleLoads = splitResult.filter(_._2.nonEmpty)

        val flexibleLoadsToRemove = onFlexibleLoads.map(_._1)
        val flexibleLoadsToAdd    = onFlexibleLoads.flatMap(_._2)

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

    }

  }

  def computePar(loads: Iterable[Load]): Double = Metric.par(AccumulatedLoad(-1, 0, loads))
  def computePar(load: Load): Double            = Metric.par(AccumulatedLoad(-1, 0, load))

}
