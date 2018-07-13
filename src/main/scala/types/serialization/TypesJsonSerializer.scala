package types.serialization

import play.api.libs.json._
import types.Types._

object TypesJsonSerializer {

  implicit val SyntheticDataTypeWrites: Writes[SyntheticDataType] =
    (o: SyntheticDataType) => Json.toJson(o.toScalaVector())

}
