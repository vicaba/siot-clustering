package types.serialization

import play.api.libs.json._
import TypesJsonSerializer._
import metrics.Metric
import types.mutable.Cluster

object ClusterJsonSerializer {

  val ClusterIdKey              = "id"
  val ClusterNameKey            = "name"
  val ClusterSyntheticCenterKey = "syntheticCenter"
  val ClusterCentriodKey        = "centroid"
  val pointsKey                 = "points"
  val metricKey                 = "metric"

  implicit val clusterWrites: Writes[Cluster] = new Writes[Cluster] {
    override def writes(o: Cluster): JsValue = Json.obj(
      ClusterIdKey              -> o.id,
      ClusterNameKey            -> o.name,
      ClusterSyntheticCenterKey -> o.syntheticValue,
      ClusterCentriodKey        -> o.centroid,
      pointsKey                 -> o.points.map(_.syntheticValue),
      metricKey                 -> Metric.par(o)
    )
  }

}
