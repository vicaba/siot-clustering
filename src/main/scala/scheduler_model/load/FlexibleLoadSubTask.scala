package scheduler_model.load

import breeze.linalg._
import scheduler_model.load.Load.{GroupId, LoadId}
import types.clusterer.DataTypeMetadata

object FlexibleLoadSubTask {

  def apply(
    id: LoadId,
    group: GroupId,
    label: String,
    startPositionInTime: LoadId,
    amplitudePerSlot: Vector[Double],
    superTask: FlexibleLoadSuperTask,
    amplitudePerSlotMetadata: DataTypeMetadata
  ): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(id, group, label, startPositionInTime, amplitudePerSlot, superTask)(amplitudePerSlotMetadata)

  def apply(
    id: LoadId,
    group: GroupId,
    label: String,
    startPositionInTime: LoadId,
    amplitudePerSlot: Vector[Double],
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
  override protected val _startPositionInTime: Int,
  override val amplitudePerSlot: Vector[Double],
  private val _superTask: FlexibleLoadSuperTask
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata)
  extends FlexibleLoad(id, group, label, _startPositionInTime, amplitudePerSlot) {

  private var __superTask: FlexibleLoadSuperTask = _superTask

  private[load] def superTask_=(task: FlexibleLoadSuperTask): FlexibleLoadSubTask = {
    __superTask = task
    this
  }

  def superTask: FlexibleLoadSuperTask = __superTask

  private[load] def copyWithoutSuperTask(): FlexibleLoadSubTask =
    FlexibleLoadSubTask(
      this.id,
      this.group,
      this.label,
      this.startPositionInTime,
      this.amplitudePerSlot,
      null,
      this.amplitudePerSlotMetadata
    )

}
