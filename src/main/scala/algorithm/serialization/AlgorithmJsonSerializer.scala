package algorithm.serialization

import algorithm.algorithms.GenAlgorithm
import play.api.libs.json._
import types.serialization.ClusterJsonSerializer._

object AlgorithmJsonSerializer {

  val SettingsKey = "settings"

  val ClustersKey = "clusters"

  implicit def stepClustererWrites[Algo <: GenAlgorithm](
      implicit settingsWrites: OWrites[Algo#ClustererSettingsT]): OWrites[Algo#StepT[Algo#ClustererSettingsT]] =
    new OWrites[Algo#StepT[Algo#ClustererSettingsT]] {
      override def writes(o: Algo#StepT[Algo#ClustererSettingsT]): JsObject = {
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
      }
    }

  implicit def stepReschedulerWrites[Algo <: GenAlgorithm](
      implicit settingsWrites: OWrites[Algo#ReschedulerSettingsT]): OWrites[Algo#StepT[Algo#ReschedulerSettingsT]] =
    new OWrites[Algo#StepT[Algo#ReschedulerSettingsT]] {
      override def writes(o: Algo#StepT[Algo#ReschedulerSettingsT]): JsObject = {
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
      }
    }
}
