package metrics.serialization

import metrics.Metric
import play.api.libs.json._

object MetricJsonSerializer {

  implicit val MetricWrites: Writes[Metric] = new Writes[Metric] {
    override def writes(o: Metric): JsValue = JsString(o.toString)
  }

}
