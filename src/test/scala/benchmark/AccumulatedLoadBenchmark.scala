package benchmark

import breeze.linalg.DenseVector
import scheduler_model.reader.SyntheticProfilesReaderForScheduler2
import org.scalameter.{Quantity, Warmer, withWarmer}
import org.scalatest.FlatSpec
import scheduler_model.load.{AccumulatedLoad, Load}
import reader.SyntheticProfilesReaderForScheduler
import types.clusterer.DataTypeMetadata

import org.scalatest.Matchers._

class AccumulatedLoadBenchmark extends FlatSpec {

  def infoTime(model: String, time: Quantity[Double]): Unit =
    info("Execution time of AccumulatedLoad.amplitudePerSlot in " + model + " load model: " + time.value + " " + time.units)

  val MainFolder = "files/syn_loads/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName = "lighting_output.csv"
  val subFoldersAndIds: List[(String, Int)] = (for (i <- 0 to 100) yield (i + "/", i)).toList

  val pointsInOldLoadModel: Seq[test.load.AccumulatedLoad] = SyntheticProfilesReaderForScheduler
    .applyDefault(MainFolder,
      subFoldersAndIds.map(_._1),
      AppliancesOutputFileName,
      LightingOutputFileName,
      subFoldersAndIds.map(_._2),
      windowSize = 30)

  val accLoadInOldLoadModel = test.load.AccumulatedLoad(-4, 0, pointsInOldLoadModel)

  val executionTimeOfAmplitudePerSlotInOldLoadModel = withWarmer(new Warmer.Default) measure {
    accLoadInOldLoadModel.amplitudePerSlot
  }

  val pointsWithNewLoadModel: Seq[scheduler_model.load.AccumulatedLoad] = SyntheticProfilesReaderForScheduler2
    .applyDefault(MainFolder,
      subFoldersAndIds.map(_._1),
      AppliancesOutputFileName,
      LightingOutputFileName,
      subFoldersAndIds.map(_._2),
      windowSize = 30)

  val accLoadInNewLoadModel = scheduler_model.load.AccumulatedLoad(-4, 0, "", pointsWithNewLoadModel)(DataTypeMetadata.generateDataTypeMetadata(forColumns = 48))

  val executionTimeOfAmplitudePerSlotInNewLoadModel = withWarmer(new Warmer.Default) measure {
    accLoadInNewLoadModel.amplitudePerSlot
  }

  executionTimeOfAmplitudePerSlotInNewLoadModel.value should be < executionTimeOfAmplitudePerSlotInOldLoadModel.value

  infoTime("old", executionTimeOfAmplitudePerSlotInOldLoadModel)
  infoTime("new", executionTimeOfAmplitudePerSlotInNewLoadModel)

}
