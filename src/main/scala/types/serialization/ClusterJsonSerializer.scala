package types.serialization

import play.api.libs.json._
import types.Cluster
import TypesJsonSerializer._

object ClusterJsonSerializer {

  val ClusterIdKey = "id"
  val ClusterNameKey = "name"
  val ClusterSyntheticCenterKey = "syntheticCenter"

  implicit val clusterWrites: Writes[Cluster] = new Writes[Cluster] {
    override def writes(o: Cluster): JsValue = Json.obj(
      ClusterIdKey -> o.id,
      ClusterNameKey -> o.name,
      ClusterSyntheticCenterKey -> o.syntheticCenter
    )
  }

}
