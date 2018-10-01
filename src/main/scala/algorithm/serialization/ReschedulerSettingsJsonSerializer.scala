package algorithm.serialization

import algorithm.clusterer.BruteClusterer
import algorithm.scheduler.ClusterRescheduler
import play.api.libs.json._
import types.serialization.TypesJsonSerializer._
import metrics.serialization.MetricJsonSerializer._

object ReschedulerSettingsJsonSerializer {

  implicit val SchedulerSettingsWrites: OWrites[ClusterRescheduler.Settings] = Json.writes[ClusterRescheduler.Settings]

}
