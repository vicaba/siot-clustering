package algorithm.serialization

import algorithm.EuclideanAlgorithm._
import play.api.libs.json._
import types.serialization.ClusterJsonSerializer._
import EuclideanClustererSettingsJsonSerializer._
import ReschedulerSettingsJsonSerializer._

object AlgorithmJsonSerializer {

  val SettingsKey = "settings"

  val ClustersKey = "clusters"

  implicit val clustererOutputWrites: OWrites[ClustererOutput] = (o: ClustererOutput) => {
      Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
    }

  implicit val reschedulerOutputWrites: OWrites[ReschedulerOutput] =
    (o: ReschedulerOutput) => {
      Json.obj(SettingsKey -> o.settings, ClustersKey -> o.clusters)
    }

  implicit val clustererAndReschedulerOutputWrites: OWrites[ClustererAndReschedulerOutput] = (o: ClustererAndReschedulerOutput) => {
    Json.obj("1" -> o.clustererOutput, "2" -> o.reschedulerOutput)
  }


}
