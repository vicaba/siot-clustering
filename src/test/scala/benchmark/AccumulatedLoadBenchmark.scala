package benchmark

import breeze.linalg.{DenseVector, sum}
import scheduler_model.reader.SyntheticProfilesReaderForScheduler2
import org.scalameter.{Quantity, Warmer, withWarmer}
import org.scalatest.FlatSpec
import scheduler_model.load.{AccumulatedLoad, Load, LoadOps}
import reader.SyntheticProfilesReaderForScheduler
import types.clusterer.DataTypeMetadata
import org.scalatest.Matchers._

class AccumulatedLoadBenchmark extends FlatSpec {

  def infoTime(model: String, method: String, time: Quantity[Double]): Unit =
    info(
      "Execution time of " + method + " in "  + model + " load model: " + time.value + " " + time.units)

  val MainFolder                            = "files/syn_loads_test/"
  val AppliancesOutputFileName              = "appliance_output.csv"
  val LightingOutputFileName                = "lighting_output.csv"
  val subFoldersAndIds: List[(String, Int)] = (for (i <- 2 to 3) yield (i + "/", i)).toList

  val pointsInOldLoadModel: Seq[test.load.AccumulatedLoad] = SyntheticProfilesReaderForScheduler
    .applyDefault(MainFolder,
                  subFoldersAndIds.map(_._1),
                  AppliancesOutputFileName,
                  LightingOutputFileName,
                  subFoldersAndIds.map(_._2),
                  windowSize = 30)

  val pointsWithNewLoadModel: Seq[scheduler_model.load.AccumulatedLoad] = SyntheticProfilesReaderForScheduler2
    .applyDefault(MainFolder,
                  subFoldersAndIds.map(_._1),
                  AppliancesOutputFileName,
                  LightingOutputFileName,
                  subFoldersAndIds.map(_._2),
                  windowSize = 30)

  val _accLoadInOldLoadModel =
    test.load.AccumulatedLoad(-1, 0, pointsInOldLoadModel.foldLeft(Set.empty[test.load.Load]) {
      case (acc, loads) =>
        acc ++ loads.loads
    })

  test.load.Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
    _accLoadInOldLoadModel,
    test.SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

  val _accLoadInNewLoadModel =
    AccumulatedLoad(1, 1, "cluster", pointsWithNewLoadModel.foldLeft(Set.empty[scheduler_model.load.Load]) {
      case (acc, loads) =>
        acc ++ loads.loads
    })(DataTypeMetadata.generateDataTypeMetadata(forColumns = 48))

  AccumulatedLoad.Mutate.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
    _accLoadInNewLoadModel,
    scheduler_model.sequence_split.SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

  var accLoadInOldLoadModel: test.load.AccumulatedLoad = _

  accLoadInOldLoadModel = _accLoadInOldLoadModel.copy()

  var accLoadInNewLoadModel: scheduler_model.load.AccumulatedLoad = _

  accLoadInNewLoadModel = LoadOps.copy(_accLoadInNewLoadModel, addSuperTaskSubTasks = true)

  val executionTimeOfCopyInOldLoadModel = withWarmer(new Warmer.Default) measure {
    accLoadInOldLoadModel = _accLoadInOldLoadModel.copy()
  }

  val executionTimeOfCopyInNewLoadModel = withWarmer(new Warmer.Default) measure {
    accLoadInNewLoadModel = LoadOps.copy(_accLoadInNewLoadModel, addSuperTaskSubTasks = true)
  }

  println("Copy")
  infoTime("old", "copy", executionTimeOfCopyInOldLoadModel)
  infoTime("new", "copy",executionTimeOfCopyInNewLoadModel)


  val executionTimeOfAmplitudePerSlotInOldLoadModel = withWarmer(new Warmer.Default) measure {
    accLoadInOldLoadModel.amplitudePerSlot
  }

  val executionTimeOfAmplitudePerSlotInNewLoadModel = withWarmer(new Warmer.Default) measure {
    accLoadInNewLoadModel.amplitudePerSlot
  }

  executionTimeOfAmplitudePerSlotInNewLoadModel.value should be < executionTimeOfAmplitudePerSlotInOldLoadModel.value

  println("Simple amplitudePerSlot")
  infoTime("old", "amplitudePerSlot", executionTimeOfAmplitudePerSlotInOldLoadModel)
  infoTime("new", "amplitudePerSlot", executionTimeOfAmplitudePerSlotInNewLoadModel)

  val executionTimeOfSliceInOldLoadModel = withWarmer(new Warmer.Default) measure {
    accLoadInOldLoadModel.amplitudePerSlot.slice(5, 10).sum
  }

  val executionTimeOfSliceInNewLoadModel = withWarmer(new Warmer.Default) measure {
    sum(accLoadInNewLoadModel.amplitudePerSlot.toDenseVector.slice(5, 10))
  }

  println("AmplitudePerSlot slice")
  infoTime("old", "slice sum", executionTimeOfSliceInOldLoadModel)
  infoTime("new", "slice sum", executionTimeOfSliceInNewLoadModel)

}
