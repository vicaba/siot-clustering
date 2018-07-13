package algorithm.serialization

import algorithm.clusterer.Clusterer
import play.api.libs.json._
import types.serialization.TypesJsonSerializer._
import metrics.serialization.MetricJsonSerializer._
import types.serialization.PointJsonSerializer

object ClustererJsonSerializer {

  implicit val SettingsWrites: OWrites[Clusterer.Settings] = {
    implicit val pointSerializer = PointJsonSerializer.syntheticPointWrites
    Json.writes[Clusterer.Settings]
  }

}
