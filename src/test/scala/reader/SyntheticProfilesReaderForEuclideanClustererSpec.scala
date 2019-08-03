package reader

import org.scalatest.{FeatureSpec, FlatSpec, GivenWhenThen}

import scala.collection.immutable.{Vector => scalaVector}
import org.scalatest.Matchers._
import org.scalatest.Matchers._
import types.clusterer.immutable.Point

import scala.io.Source
import scala.util.Try

class SyntheticProfilesReaderForEuclideanClustererSpec extends FlatSpec with GivenWhenThen {

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

  val subFoldersAndIds: List[(String, Int)] = List((0 + "/", 0))

  val resultFromReader: Vector[Point] = SyntheticProfilesReaderForEuclideanClusterer.applyDefault(
    MainFolder,
    subFoldersAndIds.map(_._1),
    AppliancesOutputFileName,
    LightingOutputFileName,
    subFoldersAndIds.map(_._2),
    windowSize = 60)

    "The first SpanSlotAccumulatedLoad.amplitudePerSlot" should "be equal to the windowed first line of the file" in {

      Given("The first user amplitude per slot read from appliances and lightning files as a Point")

      val resHead = resultFromReader.head

      And("The first user amplitude per slot read from totals file")

      val rawRead: scalaVector[Double] = readRawHeadRow(MainFolder + "0/" + "totals.csv", windowSize = 60)

      When("Totals are calculated from the Point")

      val resHeadAmplitudePerSlot = resHead.syntheticValue.toScalaVector

      Then("The amplitude per slot from the totals.csv file should be equal to the calculated totals from the Point")

      resHeadAmplitudePerSlot shouldBe rawRead
    }

    "dataLabels" should "be placed correctly" in {

      Given("The first user amplitude per slot read from appliances and lightning files as a Point")

      val resHead = resultFromReader.head

      When("Getting the number of dataLabels in the Point")

      val nDataLabels = resHead.dataLabels.length

      Then("There should be 58 labels")

      nDataLabels shouldBe 58

      And("No label is empty string")

      assert(0 == resHead.dataLabels.count(_ == ""), "no label is empty")

    }

}
