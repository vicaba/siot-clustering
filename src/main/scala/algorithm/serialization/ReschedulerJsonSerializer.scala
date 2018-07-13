package algorithm.serialization

import algorithm.clusterer.Clusterer
import algorithm.scheduler.ClusterRescheduler
import play.api.libs.json._
import types.serialization.TypesJsonSerializer._
import metrics.serialization.MetricJsonSerializer._

object ReschedulerJsonSerializer {

  implicit val SchedulerSettingsWrites: OWrites[ClusterRescheduler.Settings] = Json.writes[ClusterRescheduler.Settings]

}
