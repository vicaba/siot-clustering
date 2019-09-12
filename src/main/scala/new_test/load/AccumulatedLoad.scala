package new_test.load

import breeze.linalg.{DenseVector, sum}
import new_test.load.Load.{GroupId, LoadId}
import types.clusterer.DataTypeMetadata
import types.ops.SetOps._

import scala.collection.mutable

class AccumulatedLoad
(
  override val id: LoadId,
  override val group: GroupId,
  override val label: String,
  loads: mutable.Set[Load]
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata) extends Load {

  override def startPositionInTime: GroupId = loads.map(_.startPositionInTime).min

  override def amplitudePerSlot: DenseVector[Double] =
    if (loads.nonEmpty) sum(loads.map(_.amplitudePerSlot))
    else amplitudePerSlotMetadata.EmptySyntheticData()

}
