package scheduler_model.load

import Load._
import breeze.linalg._
import types.clusterer.DataTypeMetadata

object FixedLoad {
  def apply
  (
    id: LoadId, group: GroupId, label: String, amplitudePerSlot: Vector[Double]
  )(
    implicit amplitudePerSlotMetadata: DataTypeMetadata
  ): FixedLoad =
    new FixedLoad(id, group, label, amplitudePerSlot)

}

class FixedLoad(
  override val id: LoadId,
  override val group: GroupId,
  override val label: String,
  override val amplitudePerSlot: Vector[Double]
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata)
  extends SingleLoad {

  override def startPositionInTime: Int = 0

  def copy(): FixedLoad = FixedLoad(id, group, label, amplitudePerSlot)

  ensureCorrectCreation()

}
