package types.serialization

import play.api.libs.json.{JsValue, Writes}
import types.Point

object PointJsonSerializer {

  val syntheticPointWrites: Writes[Point] =
    (o: Point) => TypesJsonSerializer.SyntheticDataTypeWrites.writes(o.syntheticValue)

}
