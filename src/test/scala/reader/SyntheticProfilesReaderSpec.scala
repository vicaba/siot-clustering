package reader

import org.scalatest.FlatSpec
import test.SpanSlotAccumulatedLoad

import scala.io.Source
import scala.util.Try

class SyntheticProfilesReaderSpec extends FlatSpec {

  val MainFolder               = "files/syn_loads/"
  val AppliancesOutputFileName = "appliance_output.csv"
  val LightingOutputFileName   = "lighting_output.csv"

  def readRawHeadRow(l: SpanSlotAccumulatedLoad, totalsFile: String, windowSize: Int): Vector[Double] = {

    val source = Source.fromFile(totalsFile)

    Try {
      val line   = source.getLines().toList.head.split(",")
      val values = line.tail.map(_.toDouble).grouped(60).map(_.sum).toVector
      values
    }.getOrElse {
      source.close()
      Vector.empty
    }
  }

  "The first SpanSlotAccumulatedLoad.amplitudePerSlot" should "be equal to the windowed first line of the file" in {
    val subFoldersAndIds: List[(String, Int)] = List((0 + "/", 0))

    val res = SyntheticProflesReader(MainFolder,
      subFoldersAndIds.map(_._1),
      AppliancesOutputFileName,
      LightingOutputFileName,
      subFoldersAndIds.map(_._2), windowSize = 60)


    val rawRead: Vector[Double] = readRawHeadRow(res.head, MainFolder + "0/" + "totals.csv", windowSize = 60)

    assert(rawRead.head == res.head.amplitudePerSlot.head, s"${rawRead.head} was not equal to ${res.head.amplitudePerSlot.head}")
  }



}
