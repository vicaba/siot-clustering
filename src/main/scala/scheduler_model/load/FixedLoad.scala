package scheduler_model.load

import Load._
import breeze.linalg._
import types.clusterer.DataTypeMetadata

object FixedLoad {
  def apply(id: LoadId, group: GroupId, label: String, amplitudePerSlot: Vector[Double]): FixedLoad =
    new FixedLoad(id, group, label, amplitudePerSlot)
}

class FixedLoad(
    override val id: LoadId,
    override val group: GroupId,
    override val label: String,
    override val amplitudePerSlot: Vector[Double]
) extends SingleLoad {

  override val amplitudePerSlotMetadata: DataTypeMetadata =
    DataTypeMetadata.generateDataTypeMetadata(forColumns = amplitudePerSlot.length)

  override def startPositionInTime: Int = 0

  def copy(): FixedLoad = FixedLoad(id, group, label, amplitudePerSlot)

  //ensureCorrectCreation()

}
