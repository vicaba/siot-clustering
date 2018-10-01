package algorithm.serialization

import algorithm.clusterer.BruteClusterer
import play.api.libs.json._
import types.serialization.TypesJsonSerializer._
import metrics.serialization.MetricJsonSerializer._
import types.serialization.PointJsonSerializer

object BruteClustererSettingsJsonSerializer {

  implicit val SettingsWrites: OWrites[BruteClusterer.Settings] = {
    implicit val pointSerializer = PointJsonSerializer.syntheticPointWrites
    Json.writes[BruteClusterer.Settings]
  }

}
