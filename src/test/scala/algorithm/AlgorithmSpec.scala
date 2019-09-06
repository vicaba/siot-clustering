package algorithm

import batch.GenBatchRun
import metrics.{Metric, Par}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import reader.SyntheticProfilesReaderForEuclideanClusterer

class AlgorithmSpec extends FeatureSpec with GivenWhenThen {

  feature("complete algorithm process works as expected") {

    scenario("given two points clustered in one single cluster") {

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
    }
  }


}

