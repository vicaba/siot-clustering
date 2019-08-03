package types.clusterer.serialization

import play.api.libs.json._
import types.clusterer.DataTypeMetadata._

object TypesJsonSerializer {

  implicit val SyntheticDataTypeWrites: Writes[SyntheticDataType] =
    (o: SyntheticDataType) => Json.toJson(o.toScalaVector())

}
