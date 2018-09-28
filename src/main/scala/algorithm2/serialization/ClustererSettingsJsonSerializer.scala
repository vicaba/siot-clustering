package algorithm2.serialization

import algorithm2.clusterer.Clusterer
import play.api.libs.json._
import types.serialization.PointJsonSerializer
import metrics.serialization.MetricJsonSerializer._

object ClustererSettingsJsonSerializer {

  implicit val SettingsWrites: OWrites[Clusterer.Settings] = {
    implicit val pointSerializer = PointJsonSerializer.syntheticPointWrites
    Json.writes[Clusterer.Settings]
  }

}
