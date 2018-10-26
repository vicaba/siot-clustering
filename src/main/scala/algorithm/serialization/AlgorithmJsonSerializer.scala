package algorithm.serialization

import algorithm.algorithms.GenAlgorithm
import play.api.libs.json._
import types.serialization.ClusterJsonSerializer._

object AlgorithmJsonSerializer {

  val SettingsKey = "settings"

  val ClustersKey = "clusters"

  implicit def stepClustererWrites[Algo <: GenAlgorithm](
      implicit settingsWrites: OWrites[Algo#ClustererSettings]): OWrites[Algo#StepT[Algo#ClustererSettings]] =
    new OWrites[Algo#StepT[Algo#ClustererSettings]] {
      override def writes(o: Algo#StepT[Algo#ClustererSettings]): JsObject = {
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
      }
    }

  implicit def stepReschedulerWrites[Algo <: GenAlgorithm](
      implicit settingsWrites: OWrites[Algo#ReschedulerSettings]): OWrites[Algo#StepT[Algo#ReschedulerSettings]] =
    new OWrites[Algo#StepT[Algo#ReschedulerSettings]] {
      override def writes(o: Algo#StepT[Algo#ReschedulerSettings]): JsObject = {
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
      }
    }
}
