package algorithm

import batch.GenBatchRun
import metrics.{Metric, Par}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import reader.SyntheticProfilesReaderForEuclideanClusterer
import types.clusterer.immutable.Point

class AlgorithmSpec extends FeatureSpec with GivenWhenThen {

  feature("complete algorithm process works as expected") {

/*    scenario("given two points clustered in one single cluster") {

      Given("Some points")

      val MainFolder = "files/syn_loads_test/"
      val AppliancesOutputFileName = "appliance_output.csv"
      val LightingOutputFileName = "lighting_output.csv"
      val subFoldersAndIds: List[(String, Int)] = (for (i <- 2 to 3) yield (i + "/", i)).toList

      val points = SyntheticProfilesReaderForEuclideanClusterer
        .applyDefault(MainFolder,
          subFoldersAndIds.map(_._1),
          AppliancesOutputFileName,
          LightingOutputFileName,
          subFoldersAndIds.map(_._2),
          windowSize = 30)

      And("Algorithm settings")

      val testBatchRunSettingsBuilder =
        new BatchRunSettingsBuilder(points,
          List(2),
          List(Par.withParAggregate),
          (_, _) => 1)

      When("clustered and rescheduled")

      val stepsList = GenBatchRun(testBatchRunSettingsBuilder.build)

      Then("PAR should be lower")

      Metric.par(stepsList.head.clustererOutput.clusters.head) should be > Metric.par(stepsList.head.reschedulerOutput.clusters.head)
    }*/

    scenario("given six points clustered in multiple clusters") {

      def execute(numberOfClusters: Int, points: Vector[Point]): Unit = {

        Given("Some points")


        And("Algorithm settings")

        val testBatchRunSettingsBuilder =
          new BatchRunSettingsBuilder(points,
            List(numberOfClusters),
            List(Par.withParAggregate),
            (_, _) => 1)

        When("clustered and rescheduled")

        val stepsList = GenBatchRun(testBatchRunSettingsBuilder.build)

        Then("PAR should be lower")

        val unscheduledLoadsPar = Metric.par(stepsList.head.clustererOutput.clusters)
        val scheduledLoadsPar = Metric.par(stepsList.head.reschedulerOutput.clusters)

        println(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
        println(s"PAR for scheduled loads: $scheduledLoadsPar.")

        info(s"PAR for unscheduled loads: $unscheduledLoadsPar.")
        info(s"PAR for scheduled loads: $scheduledLoadsPar.")

        scheduledLoadsPar should be < unscheduledLoadsPar

      }

      val MainFolder = "files/syn_loads/"
      val AppliancesOutputFileName = "appliance_output.csv"
      val LightingOutputFileName = "lighting_output.csv"
      val subFoldersAndIds: List[(String, Int)] = (for (i <- 0 to 5) yield (i + "/", i)).toList

      val points = SyntheticProfilesReaderForEuclideanClusterer
        .applyDefault(MainFolder,
          subFoldersAndIds.map(_._1),
          AppliancesOutputFileName,
          LightingOutputFileName,
          subFoldersAndIds.map(_._2),
          windowSize = 30)

      for (i <- 1 to 1) {
        execute(i, points)
      }
    }

  }


}

