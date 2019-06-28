package reader

import test.{SingleLoad, SpanSlotAccumulatedLoad, SpanSlotFixedLoad, SpanSlotFlexibleLoad}

import scala.collection.immutable
import scala.io.Source
import scala.util.{Random, Try}

object SyntheticProflesReader {

  def apply(mainFolder: String,
            subFolders: Iterable[String],
            applianceOutputFileName: String,
            lightingOutputFileName: String,
            ids: Iterable[Int],
            windowSize: Int): Vector[SpanSlotAccumulatedLoad] =
    readSyntheticProfiles(mainFolder, subFolders, applianceOutputFileName, lightingOutputFileName, ids, windowSize)

  def readSyntheticLoads(applianceOutputFile: String,
                         lightingOutputFile: String,
                         windowSize: Int): Vector[SingleLoad] = {

    var counter = 0

    trait LoadBuilder extends ((Int, Vector[Double], String) => SingleLoad) {
      def apply(id: Int, values: Vector[Double], label: String): SingleLoad
    }

    class RandomLoadBuilder extends LoadBuilder {
      override def apply(id: Int, values: Vector[Double], label: String): SingleLoad =
        if (Random.nextBoolean()) {
          SpanSlotFixedLoad(id, 0, values, label)
        } else {
          SpanSlotFlexibleLoad(id, 0, values, label)
        }
    }

    class FixedLoadBuilder extends LoadBuilder {
      override def apply(id: Int, values: Vector[Double], label: String): SingleLoad =
        SpanSlotFixedLoad(id, 0, values, label)
    }

    def readCsv(file: String, loadBuilder: LoadBuilder)(id: Int): (Int, Vector[SingleLoad]) = {

      var idC    = id
      val source = Source.fromFile(file)

      Try {
        (for {
          line <- source.getLines()
        } yield {
          idC = idC + 1
          val items  = line.split(",").toList
          val label  = items.head
          val values = items.tail.map(_.toDouble).grouped(windowSize).map(_.sum).toVector
          loadBuilder.apply(idC, values, label)
        }).toVector
      }.map((idC, _))
        .getOrElse {
          source.close()
          (idC, Vector.empty)
        }
    }

    def rec(fs: List[Int => (Int, Vector[SingleLoad])], accId: Int, accLoads: Vector[SingleLoad]): Vector[SingleLoad] =
      fs match {
        case x :: xs =>
          val (idC, loads) = x(accId)
          rec(xs, idC, loads ++: accLoads)
        case Nil => accLoads
      }

    rec(
      List(
        readCsv(applianceOutputFile, new RandomLoadBuilder),
        readCsv(lightingOutputFile, new FixedLoadBuilder)
      ),
      accId = 0,
      Vector.empty[SingleLoad]
    )

  }

  def partitionSpanSlotFlexibleLoad(idC: Int, fl: SpanSlotFlexibleLoad, partitionBy: Double => Boolean): Unit = {

    val positionInT = 0

    val amplitudePerSlot = fl.amplitudePerSlot

    val (extracted, remainingWithZeroes) = amplitudePerSlot.partition(partitionBy)
    val newPositionInT = positionInT + extracted.size
    val newSpanSlotFlexibleLoad = SpanSlotFlexibleLoad(idC, newPositionInT, extracted)
    val (consecutiveZeroes, remainingInfo) = remainingWithZeroes.partition(partitionBy)



  }

  /**
    *
    * Reads files applianceOutputFileName, lightingOutputFileName for each profile.
    * Directory structure is prepared to be mainFolder/{subFolder}/{applianceOutputFileName|lightingOutputFileName}.
    * Data format is in CSV where there are no column headers and each row starts with a label. The label of the appliance.
    * Data is expected to be synthetically generated by https://github.com/keirstead-group/simelec .
    *
    * @param mainFolder
    * @param subFolders
    * @param applianceOutputFileName
    * @param lightingOutputFileName
    * @param ids
    * @param windowSize
    * @return
    */
  def readSyntheticProfiles(mainFolder: String,
                            subFolders: Iterable[String],
                            applianceOutputFileName: String,
                            lightingOutputFileName: String,
                            ids: Iterable[Int],
                            windowSize: Int): Vector[SpanSlotAccumulatedLoad] = {
    assert(subFolders.size == ids.size, "the number of subFolders is not equal to the number of ids")

    subFolders
      .zip(ids)
      .map {
        case (subFolder, id) =>
          val l = SpanSlotAccumulatedLoad(id,
                                          0,
                                          readSyntheticLoads(
                                            mainFolder + subFolder + applianceOutputFileName,
                                            mainFolder + subFolder + lightingOutputFileName,
                                            windowSize
                                          ))
          l

      }
      .toVector
  }

}
