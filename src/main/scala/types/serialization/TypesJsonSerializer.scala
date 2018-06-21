package types.serialization

import play.api.libs.json._
import types.Types._

object TypesJsonSerializer {

  implicit val SyntheticDataTypeWrites: Writes[SyntheticDataType] = new Writes[SyntheticDataType] {
    override def writes(o: SyntheticDataType): JsValue = Json.toJson(o.toScalaVector())
  }


}
