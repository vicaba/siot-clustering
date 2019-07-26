package reader

import breeze.linalg._
import scala.collection.immutable.{Vector => scalaVector}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import test.{SequenceSplitByConsecutiveElements, SpanSlotAccumulatedLoad}
import scala.io.Source
import scala.util.Try

class SyntheticProfilesReaderForEuclideanClustererSpec extends FlatSpec {

  val MainFolder               = "files/syn_loads_test/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName   = "lighting_output.csv"

  def readRawHeadRow(totalsFile: String, windowSize: Int): scalaVector[Double] = {

    val source = Source.fromFile(totalsFile)

    Try {
      val line   = source.getLines().toList.head.split(",")
      val values = line.tail.map(_.toDouble).grouped(windowSize).map(_.sum).toVector
      values
    }.getOrElse {
      source.close()
      scalaVector.empty
    }
  }

  "The first SpanSlotAccumulatedLoad.amplitudePerSlot" should "be equal to the windowed first line of the file" in {
    val subFoldersAndIds: List[(String, Int)] = List((0 + "/", 0))

    val res = SyntheticProfilesReaderForEuclideanClusterer.applyDefault(MainFolder,
                                                                        subFoldersAndIds.map(_._1),
                                                                        AppliancesOutputFileName,
                                                                        LightingOutputFileName,
                                                                        subFoldersAndIds.map(_._2),
                                                                        windowSize = 60)

    val resHeadAmplitudePerSlot = res.head.syntheticValue.toScalaVector

    val rawRead: scalaVector[Double] = readRawHeadRow(MainFolder + "0/" + "totals.csv", windowSize = 60)

    assert(rawRead == resHeadAmplitudePerSlot, s"$rawRead was not equal to $resHeadAmplitudePerSlot")
  }

  // TODO: Check if dataLabels are placed correctly
  "Partitioning the first SpanSlotAccumulatedLoad" should "split flexible loads" in {
    val subFoldersAndIds: List[(String, Int)] = (for (i <- 0 to 1) yield (i + "/", i)).toList

    val res = SyntheticProfilesReaderForScheduler.applyDefault(MainFolder,
                                                               subFoldersAndIds.map(_._1),
                                                               AppliancesOutputFileName,
                                                               LightingOutputFileName,
                                                               subFoldersAndIds.map(_._2),
                                                               windowSize = 30)

    val flexibleLoad =
      res.head.flexibleLoads.filter(_.label == SyntheticProfilesReaderForScheduler.Appliances.WashingMachine).head
    SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount(flexibleLoad.amplitudePerSlot) should not be empty

  }

}
