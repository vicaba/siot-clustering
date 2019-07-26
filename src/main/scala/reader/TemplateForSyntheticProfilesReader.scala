package reader

import scala.io.Source
import scala.util.Try


trait TemplateForSyntheticProfilesReader {

  type SingleLoadOutputType

  type AccumulatedLoadOutputType

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

  trait LoadBuilder extends ((Int, Vector[Double], String) => Seq[SingleLoadOutputType]) {
    def apply(id: Int, values: Vector[Double], label: String): Seq[SingleLoadOutputType]
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

  def readSyntheticLoads(applianceFileAndBuilder: LoadFileAndLoadBuilder,
    lightingFileAndBuilder: LoadFileAndLoadBuilder,
    windowSize: Int): Vector[SingleLoadOutputType] = {

    def readCsv(fileAndBuilder: LoadFileAndLoadBuilder)(id: Int): (Int, Vector[SingleLoadOutputType]) = {

      val file = fileAndBuilder.file
      val loadBuilder = fileAndBuilder.loadBuilder

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

    def rec(fs: List[Int => (Int, Vector[SingleLoadOutputType])], accId: Int, accLoads: Vector[SingleLoadOutputType]): Vector[SingleLoadOutputType] =
      fs match {
        case x :: xs =>
          val (idC, loads) = x(accId)
          rec(xs, idC, loads ++: accLoads)
        case Nil => accLoads
      }

    rec(
      List(
        readCsv(applianceFileAndBuilder),
        readCsv(lightingFileAndBuilder)
      ),
      accId = 0,
      Vector.empty[SingleLoadOutputType]
    )

  }

}
