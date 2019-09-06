package test

import algorithm.scheduler.Scheduler
import org.scalatest._
import collection.CollecctionHelper._
import metrics.Metric
import org.scalatest.Matchers._
import reader.{SyntheticProfilesReaderForEuclideanClusterer, SyntheticProfilesReaderForScheduler}
import test.load.{AccumulatedLoad, FixedLoad, FlexibleLoad, FlexibleLoadSubTask, Load}
import test.reschedulermetrics.{BiasedAverageDistanceTransformation, NoTransformation}

class SchedulerAlgorithmSpec extends FeatureSpec with GivenWhenThen {

  feature("Rescheduler.rescheduleFlexibleLoad") {

    /*scenario("Rescheduler with only one flexible load") {

      Given("an SpanSlotAccumulatedLoad with only one flexible load")

      val rawFlexibleLoad = Vector[Double](1, 1, 1, 1)

      val flexibleLoad = FlexibleLoad(1, 0, rawFlexibleLoad)

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, flexibleLoad, "")

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.rescheduleFlexibleLoad(
        (spanSlotAccumulatedLoad.copy(copyFlexibleLoadSubtasks = false),
          spanSlotAccumulatedLoad.copy(copyFlexibleLoadSubtasks = false)),
        (flexibleLoad, flexibleLoad),
        metricTransformation = NoTransformation
      )

      Then("nothing should have happened")

      result.amplitudePerSlot should equal(flexibleLoad.amplitudePerSlot)
    }

    scenario("Rescheduler with two flexible loads") {

      Given("an SpanSlotAccumulatedLoad with two loads")

      val rawFlexibleLoads = List(
        Vector[Double](1, 1, 1, 2),
        Vector[Double](2, 2, 2, 1)
      )

      val flexibleLoads = List(
        FlexibleLoad(1, 0, rawFlexibleLoads(0)),
        FlexibleLoad(2, 4, rawFlexibleLoads(1))
      )

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, flexibleLoads)

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.rescheduleFlexibleLoad(
        (spanSlotAccumulatedLoad.copy(copyFlexibleLoadSubtasks = false),
          spanSlotAccumulatedLoad.copy(copyFlexibleLoadSubtasks = false)),
        (flexibleLoads(1), flexibleLoads(1)),
        metricTransformation = NoTransformation
      )

      Then("nothing should have happened")

      Metric.par(result) should be <= Metric.par(spanSlotAccumulatedLoad)

    }

  }

  feature("Rescheduler.reschedule") {

    scenario("Rescheduler with one flexible load") {

      Given("an SpanSlotAccumulatedLoad with only one flexible load")

      val rawFlexibleLoad = Vector[Double](1, 1, 1, 1)

      val flexibleLoad = FlexibleLoad(1, 0, rawFlexibleLoad)

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, flexibleLoad, "")

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.reschedule(spanSlotAccumulatedLoad, metricTransformation = NoTransformation)

      Then("nothing should have happened")

      result.amplitudePerSlot should equal(flexibleLoad.amplitudePerSlot)
    }

    scenario("Rescheduler with one fixed load and one flexible load") {

      Given("an SpanSlotAccumulatedLoad with two loads")

      val loads = List(
        FlexibleLoad(1, 0, Vector[Double](1, 1)),
        FixedLoad(0, 0, Vector[Double](2, 1, 1, 2))
      )

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, loads)

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.reschedule(spanSlotAccumulatedLoad, metricTransformation = NoTransformation)

      Then("nothing should have happened")

      Metric.par(result) should be <= Metric.par(spanSlotAccumulatedLoad)

    }

    scenario("Rescheduler with two flexible loads and one fixed load as base") {

      Given("an SpanSlotAccumulatedLoad with three loads")

      val rawFlexibleLoads = List(
        Vector[Double](1, 1, 1, 2),
        Vector[Double](2, 2, 2, 1)
      )

      val loads = List(
        FlexibleLoad(1, 0, rawFlexibleLoads(0)),
        FlexibleLoad(2, 4, rawFlexibleLoads(1)),
        FixedLoad(3, 0, Vector[Double](1, 1, 1, 1, 1, 1, 1, 1))
      )

      val spanSlotAccumulatedLoad = AccumulatedLoad(0, 0, loads)

      When("the Rescheduler is called")

      val result = SchedulerAlgorithm.reschedule(spanSlotAccumulatedLoad, metricTransformation = NoTransformation)

      Then("nothing should have happened")

      Metric.par(result) shouldEqual Metric.par(spanSlotAccumulatedLoad)

    }*/

    scenario("given two points clustered in one single cluster") {

      Given("Some points")

      val MainFolder = "files/syn_loads_test/"
      val AppliancesOutputFileName = "appliance_output.csv"
      val LightingOutputFileName = "lighting_output.csv"
      val subFoldersAndIds: List[(String, Int)] = (for (i <- 2 to 3) yield (i + "/", i)).toList

      val _unscheduledLoads = SyntheticProfilesReaderForScheduler
        .applyDefault(MainFolder,
          subFoldersAndIds.map(_._1),
          AppliancesOutputFileName,
          LightingOutputFileName,
          subFoldersAndIds.map(_._2),
          windowSize = 30)

      val unscheduledLoads = List(
        AccumulatedLoad(-1, 0, _unscheduledLoads.foldLeft(Set.empty[Load]) { case (acc, loads) =>
          acc ++ loads.loads
        })
      )

      unscheduledLoads.foreach(
        Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
          _,
          SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage))

      When("Scheduling loads using SchedulerAlgorithm")

      val scheduledLoad =
        SchedulerAlgorithm.reschedule(Load.deepCopy(unscheduledLoads).toList.head, metricTransformation = new BiasedAverageDistanceTransformation)

      Then("ScheduledLoads PAR is lower than UnscheduledLoads PAR.")

      val unscheduledLoadsPar = Metric.par(unscheduledLoads)
      val scheduledLoadsPar = Metric.par(scheduledLoad)

      scheduledLoadsPar should be < unscheduledLoadsPar

      info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
      info(s"PAR for scheduled loads: $scheduledLoadsPar.")

      And("scheduledLoads total energy is equal to unscheduledLoads total energy")

      scheduledLoad.totalEnergy shouldBe unscheduledLoads.map(_.totalEnergy).sum

    }
  }
}
