package algorithm.serialization

import algorithm.algorithms.euclidean.EuclideanAlgorithm
import algorithm.serialization.EuclideanClustererSettingsJsonSerializer._
import algorithm.serialization.ReschedulerSettingsJsonSerializer._
import types.serialization.ClusterJsonSerializer._
import play.api.libs.json._

object EuclideanAlgorithmJsonSerializer {

  val SettingsKey = "settings"

  val ClustersKey = "clusters"

  implicit val stepTWrites: OWrites[EuclideanAlgorithm.StepT[EuclideanAlgorithm.ClustererSettings]] =
    new OWrites[EuclideanAlgorithm.StepT[EuclideanAlgorithm.ClustererSettings]] {
      override def writes(o: EuclideanAlgorithm.StepT[EuclideanAlgorithm.ClustererSettings]): JsObject = {
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
      }

    }

  implicit val stepT2Writes: OWrites[EuclideanAlgorithm.StepT[EuclideanAlgorithm.ReschedulerSettings]] =
    new OWrites[EuclideanAlgorithm.StepT[EuclideanAlgorithm.ReschedulerSettings]] {
      override def writes(o: EuclideanAlgorithm.StepT[EuclideanAlgorithm.ReschedulerSettings]): JsObject =
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
    }

  implicit val stepsWrites: OWrites[EuclideanAlgorithm.Steps] = new OWrites[EuclideanAlgorithm.Steps] {
    override def writes(o: EuclideanAlgorithm.Steps): JsObject =
      Json.obj(
        "1" -> o._1,
        "2" -> o._2
      )
  }

}
