package new_test.load

import breeze.linalg.DenseVector
import new_test.load.Load.{GroupId, LoadId}
import types.clusterer.DataTypeMetadata

object FlexibleLoadSubTask {
  def apply(
    id: LoadId,
    group: GroupId,
    label: String,
    startPositionInTime: LoadId,
    amplitudePerSlot: DenseVector[Double],
    superTask: FlexibleLoadSuperTask
  )(implicit amplitudePerSlotMetadata: DataTypeMetadata
  ): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(id, group, label, startPositionInTime, amplitudePerSlot, superTask)
}

class FlexibleLoadSubTask private(
  override val id: LoadId,
  override val group: GroupId,
  override val label: String,
  private val _startPositionInTime: Int,
  override val amplitudePerSlot: DenseVector[Double],
  private val _superTask: FlexibleLoadSuperTask
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata)
  extends FlexibleLoad(id, group, label, amplitudePerSlot) {

  private[load] var __superTask: FlexibleLoadSuperTask = _superTask

  private var __startPositionInTime: Int = _startPositionInTime

  def superTask_=(task: FlexibleLoadSuperTask): FlexibleLoadSubTask = {
    __superTask = task
    this
  }

  def superTask: FlexibleLoadSuperTask = __superTask

  def startPositionInTime_=(pos: Int): FlexibleLoadSubTask = {
    __startPositionInTime = pos
    this
  }

  override def startPositionInTime: Int = __startPositionInTime

}
