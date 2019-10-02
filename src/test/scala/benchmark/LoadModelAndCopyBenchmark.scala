package benchmark

import reader.SyntheticProfilesReaderForScheduler
import org.scalameter.{Quantity, Warmer, withWarmer}
import org.scalatest.FlatSpec
import scheduler_model.load.LoadOps
import scheduler_model.reader.SyntheticProfilesReaderForScheduler2
import types.clusterer.DataTypeMetadata


class LoadModelAndCopyBenchmark extends FlatSpec {

  val MainFolder = "files/syn_loads_test/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName = "lighting_output.csv"
  val subFoldersAndIds: List[(String, Int)] = (for (i <- 2 to 3) yield (i + "/", i)).toList

  val _unscheduledLoadsOld = SyntheticProfilesReaderForScheduler
    .applyDefault(MainFolder,
      subFoldersAndIds.map(_._1),
      AppliancesOutputFileName,
      LightingOutputFileName,
      subFoldersAndIds.map(_._2),
      windowSize = 30)

  val oldModel = withWarmer(new Warmer.Default) measure {

    val unscheduledLoads = List(
      test.load.AccumulatedLoad(-1, 0, _unscheduledLoadsOld.foldLeft(Set.empty[test.load.Load]) { case (acc, loads) =>
        acc ++ loads.loads
      })
    )

    unscheduledLoads.foreach(
      test.load.Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
        _,
        test.SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage))

    test.load.Load.deepCopy(unscheduledLoads).toList.head

  }

  val _unscheduledLoadsNew = SyntheticProfilesReaderForScheduler2
    .applyDefault(MainFolder,
      subFoldersAndIds.map(_._1),
      AppliancesOutputFileName,
      LightingOutputFileName,
      subFoldersAndIds.map(_._2),
      windowSize = 30)

  val newModel = withWarmer(new Warmer.Default) measure {

    val unscheduledLoad =
      scheduler_model.load.AccumulatedLoad(1, 1, "cluster", _unscheduledLoadsNew.foldLeft(Set.empty[scheduler_model.load.Load]) {
        case (acc, loads) =>
          acc ++ loads.loads
      })(DataTypeMetadata.generateDataTypeMetadata(forColumns = 48))

    scheduler_model.load.AccumulatedLoad.Mutate.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
      unscheduledLoad,
      scheduler_model.sequence_split.SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)


    LoadOps.copy(unscheduledLoad)
  }

  info("execution time of old model: " + oldModel.value + " " + oldModel.units)
  info("execution time of new model: " + newModel.value + " " + newModel.units)

}
