package new_test.load

import Load._
import breeze.linalg.DenseVector
import types.clusterer.DataTypeMetadata

object FlexibleLoad {
  def apply
  (
    id: LoadId, group: GroupId, label: String, amplitudePerSlot: DenseVector[Double]
  )(
    implicit amplitudePerSlotMetadata: DataTypeMetadata
  ): FlexibleLoad =
    new FlexibleLoad(id, group, label, amplitudePerSlot)
}

class FlexibleLoad(
  override val id: LoadId,
  override val group: GroupId,
  override val label: String,
  override val amplitudePerSlot: DenseVector[Double]
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata)
  extends SingleLoad {

  ensureCorrectCreation()

  override def startPositionInTime: Int = 0

}
