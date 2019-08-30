package algorithm.serialization

import play.api.libs.json._
import metrics.serialization.MetricJsonSerializer._

object SchedulerSettingsJsonSerializer {


  val NumberOfClustersKey            = "numberOfClusters"
  val metricKey                      = "metric"
  val metricTransformationKey        = "metricTransformation"
  val userOrderingsKey               = "userOrderings"
  val schedulerAlgorithmOrderingsKey = "schedulerAlgorithmOrderings"

  implicit val schedulerWrites: OWrites[algorithm.scheduler.ReschedulerSettings] =
    new OWrites[algorithm.scheduler.ReschedulerSettings] {
      override def writes(o: algorithm.scheduler.ReschedulerSettings): JsObject = Json.obj(
        NumberOfClustersKey            -> o.numberOfClusters,
        metricKey                      -> o.metric,
        metricTransformationKey        -> o.metricTransformation.toString(),
        userOrderingsKey               -> o.userOrderings.size,
        schedulerAlgorithmOrderingsKey -> o.schedulerAlgorithmOrderings.size,
      )
    }

}
