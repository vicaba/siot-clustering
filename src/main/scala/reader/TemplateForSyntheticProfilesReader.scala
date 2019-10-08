package reader

import scheduler_model.load.Load.LoadId

import scala.io.Source
import scala.util.Try

object TemplateForSyntheticProfilesReader {

  import Appliances._

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

  val FlexibleLoads: List[String] = List(
    DishWasher,
    TumbleDryer,
    WashingMachine,
    WasherDryer
  )

}


trait TemplateForSyntheticProfilesReader {

  type SingleLoadOutputType

  type AccumulatedLoadOutputType


  trait LoadBuilder extends ((Int, Vector[Double], String, Option[String]) => SingleLoadOutputType) {
    def apply(id: Int, values: Vector[Double], label: String, replaceWithLabel: Option[String] = None): SingleLoadOutputType
  }

  case class LoadFileAndLoadBuilder(file: String, loadBuilder: LoadBuilder)


  def applyDefault(mainFolder: String,
    subFolders: Iterable[String],
    applianceOutputFileName: String,
    lightingOutputFileName: String,
    ids: Iterable[Int],
    windowSize: Int): Vector[AccumulatedLoadOutputType]

  /**
    *
    * Reads files applianceOutputFileName, lightingOutputFileName for each profile.
    * Directory structure is prepared to be mainFolder/{subFolder}/{applianceOutputFileName|lightingOutputFileName}.
    * Data format is in CSV where there are no column headers and each row starts with a label. The label of the appliance.
    * Data is expected to be synthetically generated by https://github.com/keirstead-group/simelec .
    *
    * @param mainFolder
    * @param subFolders
    * @param applianceFileAndBuilder
    * @param lightingFileAndBuilder
    * @param ids
    * @param windowSize
    * @return
    */
  def apply(mainFolder: String,
    subFolders: Iterable[String],
    applianceFileAndBuilder: String => LoadFileAndLoadBuilder,
    lightingFileAndBuilder: String => LoadFileAndLoadBuilder,
    ids: Iterable[Int],
    windowSize: Int): Vector[AccumulatedLoadOutputType]

  /**
    *
    * @param applianceFileAndBuilder
    * @param lightingFileAndBuilder
    * @param windowSize
    * @param idC the last used id for a load
    * @return
    */
  def readSyntheticLoads(applianceFileAndBuilder: LoadFileAndLoadBuilder,
    lightingFileAndBuilder: LoadFileAndLoadBuilder,
    windowSize: Int, idC: LoadId): (Vector[SingleLoadOutputType], LoadId) = {

    def readCsv(fileAndBuilder: LoadFileAndLoadBuilder)(id: LoadId): (Vector[SingleLoadOutputType], LoadId) = {

      val file = fileAndBuilder.file
      val loadBuilder = fileAndBuilder.loadBuilder

      var idC    = id

      //TODO: Review this part, exception and file close is not properly handled
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
      }.map((_, idC))
        .getOrElse {
          source.close()
          (Vector.empty, idC)
        }
    }

    def rec(fs: List[Int => (Vector[SingleLoadOutputType], LoadId)], accId: LoadId, accLoads: Vector[SingleLoadOutputType]): (Vector[SingleLoadOutputType], LoadId) =
      fs match {
        case x :: xs =>
          val (loads, idC) = x(accId)
          rec(xs, idC, loads ++: accLoads)
        case Nil => (accLoads, accId)
      }

    rec(
      List(
        readCsv(applianceFileAndBuilder),
        readCsv(lightingFileAndBuilder)
      ),
      accId = idC,
      Vector.empty[SingleLoadOutputType]
    )

  }

}
