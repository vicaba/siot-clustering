package reader

import test.{SingleLoad, SpanSlotAccumulatedLoad, SpanSlotFixedLoad, SpanSlotFlexibleLoad}

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Random, Try}

object SyntheticProfilesReader {

  object Appliances {
    val FridgeFreezer    = "FRIDGE_FREEZER"
    val Fridge           = "FRIDGE"
    val UprightFreezer   = "UPRIGHT_FREEZER"
    val AnswerMachine    = "ANSWER_MACHINE"
    val CdPlayer         = "CD_PLAYER"
    val Clock            = "CLOCK"
    val Phone            = "PHONE"
    val Hifi             = "HIFI"
    val Iron             = "IRON"
    val Vacuum           = "VACUUM"
    val Fax              = "FAX"
    val Pc               = "PC"
    val Printer          = "PRINTER"
    val Tv1              = "TV1"
    val Tv2              = "TV2"
    val Tv3              = "TV3"
    val VcrDvd           = "VCR_DVD"
    val Receiver         = "RECEIVER"
    val Hob              = "HOB"
    val Oven             = "OVEN"
    val Microwave        = "MICROWAVE"
    val Kettle           = "KETTLE"
    val SmallCooking     = "SMALL_COOKING"
    val DishWasher       = "DISH_WASHER"
    val TumbleDryer      = "TUMBLE_DRYER"
    val WashingMachine   = "WASHING_MACHINE"
    val WasherDryer      = "WASHER_DRYER"
    val Deswh            = "DESWH"
    val EInst            = "E_INST"
    val ElecShower       = "ELEC_SHOWER"
    val StorageHeater    = "STORAGE_HEATER"
    val ElecSpaceHeating = "ELEC_SPACE_HEATING"

  }

  def apply(mainFolder: String,
            subFolders: Iterable[String],
            applianceOutputFileName: String,
            lightingOutputFileName: String,
            ids: Iterable[Int],
            windowSize: Int): Vector[SpanSlotAccumulatedLoad] =
    readSyntheticProfiles(mainFolder, subFolders, applianceOutputFileName, lightingOutputFileName, ids, windowSize)

  def splitSequenceBySequenceOfElements[E](seq: Seq[E], sequenceOfElementsValue: E): Seq[(Int, Seq[E])] = {

    case class Extracted(extractedSeq: Seq[E], remainingSeq: Seq[E])

    @tailrec
    def _extractSequenceOfElements(_seq: Seq[E], accum: mutable.ListBuffer[E] = new ListBuffer[E], comparator: (E, E) => Boolean): Extracted = {
      if (_seq.isEmpty) return Extracted(accum, _seq)
      if (comparator(_seq.head, sequenceOfElementsValue))
        _extractSequenceOfElements(_seq.tail, accum += _seq.head, comparator)
      else Extracted(accum, _seq)
    }

    @tailrec
    def _splitSequenceBySequenceOfElements(index: Int,
                                           remainingSeq: Seq[E],
                                           accum: Seq[(Int, Seq[E])]): Seq[(Int, Seq[E])] = {
      if (remainingSeq.isEmpty) return accum
      if (remainingSeq.head == sequenceOfElementsValue) {
        val extracted = _extractSequenceOfElements(remainingSeq, new ListBuffer[E], (e1, e2) => e1 == e2)
        _splitSequenceBySequenceOfElements(index + extracted.extractedSeq.length, extracted.remainingSeq, accum)
      } else {
        val extracted = _extractSequenceOfElements(remainingSeq, new ListBuffer[E], (e1, e2) => e1 != e2)
        _splitSequenceBySequenceOfElements(index + extracted.extractedSeq.length,
                                           extracted.remainingSeq,
                                           (index, extracted.extractedSeq) +: accum)
      }

    }

    _splitSequenceBySequenceOfElements(index = 0, seq, Seq())

  }

  def readSyntheticLoads(applianceOutputFile: String,
                         lightingOutputFile: String,
                         windowSize: Int): Vector[SingleLoad] = {

    trait LoadBuilder extends ((Int, Vector[Double], String) => Seq[SingleLoad]) {
      def apply(id: Int, values: Vector[Double], label: String): Seq[SingleLoad]
    }

    class ApplianceLoadBuilder extends LoadBuilder {
      import Appliances._
      override def apply(id: Int, values: Vector[Double], label: String): Seq[SingleLoad] =
        List(label match {
          case DishWasher     => SpanSlotFlexibleLoad(id, 0, values, label)
          case TumbleDryer    => SpanSlotFlexibleLoad(id, 0, values, label)
          case WashingMachine => SpanSlotFlexibleLoad(id, 0, values, label)
          case WasherDryer    => SpanSlotFlexibleLoad(id, 0, values, label)
          case _              => SpanSlotFixedLoad(id, 0, values, label)
        })
    }

    class FixedLoadBuilder extends LoadBuilder {
      override def apply(id: Int, values: Vector[Double], label: String): Seq[SingleLoad] =
        List(SpanSlotFixedLoad(id, 0, values, label))
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
        }).toVector.flatten
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
        //TODO: Change RandomLadBuilder so it always builds the same fixed and flexible loads by label
        readCsv(applianceOutputFile, new ApplianceLoadBuilder),
        readCsv(lightingOutputFile, new FixedLoadBuilder)
      ),
      accId = 0,
      Vector.empty[SingleLoad]
    )

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
