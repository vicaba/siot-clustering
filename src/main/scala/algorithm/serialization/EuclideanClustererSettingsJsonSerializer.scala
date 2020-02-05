package algorithm.serialization

import algorithm.clusterer.{EuclideanClusterer, EuclideanClustererSettings}
import collection.serialization.ShufflerJsonSerializer
import play.api.libs.json.{Json, OWrites}
import types.clusterer.serialization.PointJsonSerializer
import metrics.serialization.MetricJsonSerializer._

object EuclideanClustererSettingsJsonSerializer {

  implicit val SettingsWrites: OWrites[EuclideanClustererSettings] = {
    implicit val pointSerializer = PointJsonSerializer.syntheticPointWrites
    implicit val shufflerWrites = ShufflerJsonSerializer.shufflerWrites
    Json.writes[EuclideanClustererSettings]
  }

}
