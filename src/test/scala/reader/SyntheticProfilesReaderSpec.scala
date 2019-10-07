package reader

import metrics.Metric
import org.scalatest.{FlatSpec, GivenWhenThen}
import org.scalatest.Matchers._
import scheduler_model.reader.SyntheticProfilesReaderForScheduler2

class SyntheticProfilesReaderSpec extends FlatSpec with GivenWhenThen {

  val MainFolder               = "files/syn_loads_test/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName   = "lighting_output.csv"

  "Both SyntheticProfilesReaders" should "read the same points" in {

    val subFoldersAndIds: List[(String, Int)] = (for (i <- 0 to 4) yield (i + "/", i)).toList

    val schedulerModelEntities = SyntheticProfilesReaderForScheduler2.applyDefault(MainFolder,
      subFoldersAndIds.map(_._1),
      AppliancesOutputFileName,
      LightingOutputFileName,
      subFoldersAndIds.map(_._2),
      windowSize = 60)

    val clustererModelEntities = SyntheticProfilesReaderForEuclideanClusterer.applyDefault(
      MainFolder,
      subFoldersAndIds.map(_._1),
      AppliancesOutputFileName,
      LightingOutputFileName,
      subFoldersAndIds.map(_._2),
      windowSize = 60)

    Metric.par(schedulerModelEntities) shouldBe Metric.par(clustererModelEntities)

  }

}
