package benchmark

import breeze.linalg.DenseVector
import org.scalameter.Warmer
import org.scalameter.withWarmer
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import reader.{SyntheticProfilesReaderForEuclideanClusterer, SyntheticProfilesReaderForScheduler}
import test.SequenceSplitByConsecutiveElements
import test.load.{AccumulatedLoad, Load}
import types.clusterer
import types.clusterer.DataTypeMetadata.SyntheticDataType
import types.clusterer.immutable.Point

import scala.annotation.tailrec
import scala.collection.immutable

class AccumulatedLoadBenchmark extends FlatSpec {

  val MainFolder = "files/syn_loads/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName = "lighting_output.csv"
  val subFoldersAndIds: List[(String, Int)] = (for (i <- 0 to 199) yield (i + "/", i)).toList

  val points: Seq[AccumulatedLoad] = SyntheticProfilesReaderForScheduler
    .applyDefault(MainFolder,
      subFoldersAndIds.map(_._1),
      AppliancesOutputFileName,
      LightingOutputFileName,
      subFoldersAndIds.map(_._2),
      windowSize = 30)

  val accLoad = AccumulatedLoad(-4, 0, points)

  Load.MutateAccumulatedLoad.splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(
    accLoad,
    SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage)

  val executionTimeOfAmplitudePerSlot = withWarmer(new Warmer.Default) measure {
    accLoad.amplitudePerSlot
  }

  info(executionTimeOfAmplitudePerSlot.value + " " + executionTimeOfAmplitudePerSlot.units)

}
