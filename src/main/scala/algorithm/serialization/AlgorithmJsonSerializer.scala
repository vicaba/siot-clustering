package algorithm.serialization

import algorithm.algorithms.euclidean.EuclideanAlgorithm._
import play.api.libs.json._
import types.serialization.ClusterJsonSerializer._
import EuclideanClustererSettingsJsonSerializer._
import ReschedulerSettingsJsonSerializer._

object AlgorithmJsonSerializer {

  val SettingsKey = "settings"

  val ClustersKey = "clusters"

  implicit def clustererOutputWrites(
      implicit settingsWrites: OWrites[ClustererOutput]): OWrites[ClustererOutput] =
    new OWrites[ClustererOutput] {
      override def writes(o: ClustererOutput): JsObject = {
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
      }
    }

  implicit def reschedulerOutputWrites(
      implicit settingsWrites: OWrites[ReschedulerOutput]): OWrites[ReschedulerOutput] =
    new OWrites[ReschedulerOutput] {
      override def writes(o: ReschedulerOutput): JsObject = {
        Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
      }
    }
}
