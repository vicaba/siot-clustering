package new_test.load

import breeze.linalg.{DenseVector, sum}
import new_test.load.Load.{GroupId, LoadId}
import types.clusterer.DataTypeMetadata
import collection.CollecctionHelper._

import scala.collection.mutable

object AccumulatedLoad {

  def apply(id: LoadId, group: GroupId, label: String, load: Load)
    (implicit amplitudePerSlotMetadata: DataTypeMetadata): AccumulatedLoad =
    new AccumulatedLoad(id, group, label, new scala.collection.mutable.HashSet[Load]() += load)

  def apply(id: LoadId, group: GroupId, label: String, loads: Traversable[Load])
    (implicit amplitudePerSlotMetadata: DataTypeMetadata): AccumulatedLoad =
    new AccumulatedLoad(id, group, label, mutableSetOf(loads))

  def keepLoadOrder(id: LoadId, group: GroupId, label: String, loads: Traversable[Load])
    (implicit amplitudePerSlotMetadata: DataTypeMetadata): AccumulatedLoad =
    new AccumulatedLoad(id, group, label, orderedMutableSetOf(loads))

}


class AccumulatedLoad
(
  override val id: LoadId,
  override val group: GroupId,
  override val label: String,
  loads: mutable.Set[Load]
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata) extends Load {

  override def startPositionInTime: Int = loads.map(_.startPositionInTime).min

  override def amplitudePerSlot: DenseVector[Double] =
    if (loads.nonEmpty) sum(loads.map(_.amplitudePerSlot))
    else amplitudePerSlotMetadata.EmptySyntheticData()

}
