package scheduler_model.load

import Load._
import breeze.linalg.DenseVector
import types.clusterer.DataTypeMetadata

object FixedLoad {
  def apply
  (
    id: LoadId, group: GroupId, label: String, amplitudePerSlot: DenseVector[Double]
  )(
    implicit amplitudePerSlotMetadata: DataTypeMetadata
  ): FixedLoad =
    new FixedLoad(id, group, label, amplitudePerSlot)

}

class FixedLoad(
  override val id: LoadId,
  override val group: GroupId,
  override val label: String,
  override val amplitudePerSlot: DenseVector[Double]
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata)
  extends SingleLoad {

  ensureCorrectCreation()

  override def startPositionInTime: Int = 0

  def copy(): FixedLoad = FixedLoad(id, group, label, amplitudePerSlot)

}
