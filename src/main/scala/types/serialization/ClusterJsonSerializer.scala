package types.serialization

import play.api.libs.json._
import types.Cluster
import TypesJsonSerializer._
import metrics.Metric

object ClusterJsonSerializer {

  val ClusterIdKey = "id"
  val ClusterNameKey = "name"
  val ClusterSyntheticCenterKey = "syntheticCenter"
  val pointsKey = "points"
  val metricKey = "metric"

  implicit val clusterWrites: Writes[Cluster] = new Writes[Cluster] {
    override def writes(o: Cluster): JsValue = Json.obj(
      ClusterIdKey -> o.id,
      ClusterNameKey -> o.name,
      ClusterSyntheticCenterKey -> o.syntheticCenter,
      pointsKey -> o.points.map {p => p.syntheticValue},
      metricKey -> Metric.par(o)
    )
  }

}