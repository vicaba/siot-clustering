package algorithm.serialization

import algorithm.clusterer.EuclideanClusterer
import play.api.libs.json.{Json, OWrites}
import types.serialization.PointJsonSerializer
import metrics.serialization.MetricJsonSerializer._

object EuclideanClustererSettingsJsonSerializer {

  implicit val SettingsWrites: OWrites[EuclideanClusterer.Settings] = {
    implicit val pointSerializer = PointJsonSerializer.syntheticPointWrites
    Json.writes[EuclideanClusterer.Settings]
  }

}
