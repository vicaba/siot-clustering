package types.clusterer.serialization

import play.api.libs.json.{JsValue, Writes}
import types.clusterer.immutable.Point
object PointJsonSerializer {

  val syntheticPointWrites: Writes[Point] =
    (o: Point) => TypesJsonSerializer.SyntheticDataTypeWrites.writes(o.syntheticValue)

}
