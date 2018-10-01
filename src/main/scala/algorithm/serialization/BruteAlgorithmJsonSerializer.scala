package algorithm.serialization

import algorithm.algorithms.BruteAlgorithm.{Step1, Step2, Steps}
import play.api.libs.json._
import algorithm.serialization.BruteClustererSettingsJsonSerializer._
import algorithm.serialization.ReschedulerSettingsJsonSerializer._
import types.serialization.ClusterJsonSerializer._

object BruteAlgorithmJsonSerializer {

  implicit val Step1Writes = Json.writes[Step1]

  implicit val Step2Writes = Json.writes[Step2]

  val Step1Key = "1"
  val Step2Key = "2"

  implicit val stepsWrites: Writes[Steps] = new Writes[Steps] {
    override def writes(o: Steps): JsValue = Json.arr(
      o._1,
      o._2
    )
  }
}
