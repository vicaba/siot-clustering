package algorithm.serialization

import play.api.libs.json._
import types.serialization.TypesJsonSerializer._
import metrics.serialization.MetricJsonSerializer._

object ReschedulerSettingsJsonSerializer {

  implicit val SchedulerSettingsWrites: OWrites[algorithm.scheduler.ReschedulerSettings] = Json.writes[algorithm.scheduler.ReschedulerSettings]

}
