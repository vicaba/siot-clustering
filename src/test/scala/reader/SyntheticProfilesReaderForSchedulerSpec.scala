package reader

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import test.{SpanSlotAccumulatedLoad, SequenceSplitByConsecutiveElements}

import scala.io.Source
import scala.util.Try

class SyntheticProfilesReaderForSchedulerSpec extends FlatSpec {

  val MainFolder               = "files/syn_loads_test/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName   = "lighting_output.csv"

  def readRawHeadRow(totalsFile: String, windowSize: Int): Vector[Double] = {

    val source = Source.fromFile(totalsFile)

    Try {
      val line   = source.getLines().toList.head.split(",")
      val values = line.tail.map(_.toDouble).grouped(windowSize).map(_.sum).toVector
      values
    }.getOrElse {
      source.close()
      Vector.empty
    }
  }

  "The first SpanSlotAccumulatedLoad.amplitudePerSlot" should "be equal to the windowed first line of the file" in {
    val subFoldersAndIds: List[(String, Int)] = List((0 + "/", 0))

    val res = SyntheticProfilesReaderForScheduler.applyDefault(MainFolder,
                                      subFoldersAndIds.map(_._1),
                                      AppliancesOutputFileName,
                                      LightingOutputFileName,
                                      subFoldersAndIds.map(_._2),
                                      windowSize = 60)

    val rawRead: Vector[Double] = readRawHeadRow(MainFolder + "0/" + "totals.csv", windowSize = 60)

    assert(rawRead == res.head.amplitudePerSlot,
           s"${rawRead} was not equal to ${res.head.amplitudePerSlot}")
  }

  "Partitioning the first SpanSlotAccumulatedLoad" should "split flexible loads" in {
    val subFoldersAndIds: List[(String, Int)] = (for (i <- 0 to 1) yield (i + "/", i)).toList

    val res = SyntheticProfilesReaderForScheduler.applyDefault(MainFolder,
                                      subFoldersAndIds.map(_._1),
                                      AppliancesOutputFileName,
                                      LightingOutputFileName,
                                      subFoldersAndIds.map(_._2),
                                      windowSize = 30)

    val flexibleLoad = res.head.flexibleLoads.filter(_.label == SyntheticProfilesReaderForScheduler.Appliances.WashingMachine).head
    SequenceSplitByConsecutiveElements.withConsecutiveValueAsTheHighestCount(flexibleLoad.amplitudePerSlot) should not be empty

  }

}
