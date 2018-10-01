package algorithm.serialization
import algorithm.algorithms.brute.BruteAlgorithm.{StepT, Steps}
import algorithm.algorithms.brute.BruteAlgorithm
import algorithm.clusterer.BruteClusterer
import algorithm.serialization.BruteClustererSettingsJsonSerializer._
import algorithm.serialization.ReschedulerSettingsJsonSerializer._
import types.serialization.ClusterJsonSerializer._
import play.api.libs.json._


object BruteAlgorithmJsonSerializer {

  val SettingsKey = "settings"

  val ClustersKey = "clusters"

  implicit val stepTWrites: OWrites[BruteAlgorithm.StepT[BruteAlgorithm.ClustererSettings]] =
    new OWrites[BruteAlgorithm.StepT[BruteAlgorithm.ClustererSettings]] {
      override def writes(o: BruteAlgorithm.StepT[BruteAlgorithm.ClustererSettings]): JsObject = {
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
      }

    }

  implicit val stepT2Writes: OWrites[BruteAlgorithm.StepT[BruteAlgorithm.ReschedulerSettings]] =
    new OWrites[BruteAlgorithm.StepT[BruteAlgorithm.ReschedulerSettings]] {
      override def writes(o: BruteAlgorithm.StepT[BruteAlgorithm.ReschedulerSettings]): JsObject =
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
    }

  implicit val stepsWrites: OWrites[BruteAlgorithm.Steps] = new OWrites[BruteAlgorithm.Steps] {
    override def writes(o: BruteAlgorithm.Steps): JsObject =
      Json.obj(
        "1" -> o._1,
        "2" -> o._2
      )
  }

}
