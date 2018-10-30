package algorithm.serialization

import algorithm.clusterer.FlattenedEuclideanClusterer
import play.api.libs.json.{Json, OWrites}
import types.serialization.PointJsonSerializer
import metrics.serialization.MetricJsonSerializer._

object EuclideanClustererSettingsJsonSerializer {

  implicit val SettingsWrites: OWrites[FlattenedEuclideanClusterer.Settings] = {
    implicit val pointSerializer = PointJsonSerializer.syntheticPointWrites
    Json.writes[FlattenedEuclideanClusterer.Settings]
  }

}
