package algorithm

import algorithm.scheduler.Scheduler
import metrics.Metric
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import reader.SyntheticProfilesReader
import test.{Load, SpanSlotAccumulatedLoad}
import test.reschedulermetrics.BiasedAverageDistanceTransformation

class SchedulerSpec extends FeatureSpec with GivenWhenThen {

  val MainFolder               = "files/syn_loads/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName   = "lighting_output.csv"

  feature("Scheduler. PAR is minimized after rescheduling") {

    scenario("With synthetic data, PAR is minimized after rescheduling") {

      Given("Synthetically generated data")

      val subFoldersAndIds: List[(String, Int)] = (for (i <- 0 until 200) yield (i + "/", i)).toList

      val unscheduledLoads = SyntheticProfilesReader(MainFolder,
                                       subFoldersAndIds.map(_._1),
                                       AppliancesOutputFileName,
                                       LightingOutputFileName,
                                       subFoldersAndIds.map(_._2),
                                       windowSize = 60).toList.take(2)

      val scheduledLoads = Scheduler.apply(unscheduledLoads, new BiasedAverageDistanceTransformation)

      val unscheduledLoadsPar = computePar(unscheduledLoads)
      val scheduledLoadsPar = computePar(scheduledLoads)

      scheduledLoadsPar should be < unscheduledLoadsPar

      info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
      info(s"PAR for scheduled loads: $scheduledLoadsPar.")


    }

  }

  def computePar(loads: Iterable[Load]): Double = Metric.par(SpanSlotAccumulatedLoad(-1, 0, loads))
  def computePar(load: Load): Double            = Metric.par(SpanSlotAccumulatedLoad(-1, 0, load))

}
