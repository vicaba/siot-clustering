package scheduler_model.load

import breeze.linalg.DenseVector
import scheduler_model.load.Load.{GroupId, LoadId}
import types.clusterer.DataTypeMetadata

object FlexibleLoadSubTask {

  def apply(
    id: LoadId,
    group: GroupId,
    label: String,
    startPositionInTime: LoadId,
    amplitudePerSlot: DenseVector[Double],
    superTask: FlexibleLoadSuperTask,
    amplitudePerSlotMetadata: DataTypeMetadata
  ): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(id, group, label, startPositionInTime, amplitudePerSlot, superTask)(amplitudePerSlotMetadata)

  def apply(
    id: LoadId,
    group: GroupId,
    label: String,
    startPositionInTime: LoadId,
    amplitudePerSlot: DenseVector[Double],
    superTask: FlexibleLoadSuperTask
  ): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(id, group, label, startPositionInTime, amplitudePerSlot, superTask)(DataTypeMetadata.generateDataTypeMetadata(forColumns = amplitudePerSlot.length))

  private[load] def copyWithAmplitudePerSlotToZero(f: FlexibleLoadSubTask): FlexibleLoadSubTask =
    FlexibleLoadSubTask(f.id, f.group, f.label, f.startPositionInTime, DenseVector.fill(f.span, 0.0), f.superTask, f.amplitudePerSlotMetadata)

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

  private var __superTask: FlexibleLoadSuperTask = _superTask

  private var __startPositionInTime: Int = _startPositionInTime

  private[load] def superTask_=(task: FlexibleLoadSuperTask): FlexibleLoadSubTask = {
    __superTask = task
    this
  }

  def superTask: FlexibleLoadSuperTask = __superTask

  private[load] def startPositionInTime_=(pos: Int): FlexibleLoadSubTask = {
    __startPositionInTime = pos
    this
  }

  override def startPositionInTime: Int = __startPositionInTime

}
