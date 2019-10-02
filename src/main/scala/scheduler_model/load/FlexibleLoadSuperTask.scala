package scheduler_model.load

import breeze.linalg.DenseVector
import scheduler_model.load.Load.{GroupId, LoadId}
import types.clusterer.DataTypeMetadata

import scala.util.Try

object FlexibleLoadSuperTask {

  def apply(
    id: LoadId,
    group: GroupId,
    label: String,
    amplitudeInOffStatus: Double,
    aggregatees: List[FlexibleLoadSubTask],
    computeAmplitudePerSlotWithRestValueOnly: Boolean)
    (implicit amplitudePerSlotMetadata: DataTypeMetadata
    ): FlexibleLoadSuperTask = {

    val superTask = new FlexibleLoadSuperTask(id, group, label, amplitudeInOffStatus, Nil, computeAmplitudePerSlotWithRestValueOnly)
    superTask.aggregatees = aggregatees.map(_.superTask = superTask)
    superTask
  }

}

class FlexibleLoadSuperTask(
  override val id: LoadId,
  override val group: GroupId,
  override val label: String,
  val amplitudeInOffStatus: Double,
  private val _aggregatees: List[FlexibleLoadSubTask],
  private val _computeAmplitudePerSlotWithRestValueOnly: Boolean
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata)
  extends Load {

  private var __aggregatees: List[FlexibleLoadSubTask] = _aggregatees

  private var __computeAmplitudePerSlotWithRestValueOnly: Boolean = _computeAmplitudePerSlotWithRestValueOnly

  override def startPositionInTime: Int = 0

  private[load] def aggregatees_=(aggregatees: List[FlexibleLoadSubTask]): FlexibleLoadSuperTask = {
    __aggregatees = aggregatees
    __aggregatees.foreach(_.superTask = this)
    this
  }

  def aggregatees: List[FlexibleLoadSubTask] = __aggregatees

  def computeAmplitudePerSlotWithRestValueOnly_=(opt: Boolean): FlexibleLoadSuperTask = {
    __computeAmplitudePerSlotWithRestValueOnly = opt
    this
  }

  def computeAmplitudePerSlotWithRestValueOnly: Boolean = __computeAmplitudePerSlotWithRestValueOnly

  def areAggregateesOverlapped: Boolean = LoadOps.areLoadsOverlapped(aggregatees)

  override def amplitudePerSlot: DenseVector[Double] =
    if (!computeAmplitudePerSlotWithRestValueOnly)
      LoadOps.aggregatedAmplitudePerSlot(aggregatees, amplitudeInOffStatus, amplitudePerSlotMetadata)
    else
      LoadOps.aggregatedAmplitudePerSlot(aggregatees.map(FlexibleLoadSubTask.copyWithAmplitudePerSlotToZero), amplitudeInOffStatus, amplitudePerSlotMetadata)

  private[load] def copyAndCopySubTasks(): FlexibleLoadSuperTask = {
    val subTasksCopy = this.aggregatees.map(_.copyWithoutSuperTask())
    val superTaskCopy = FlexibleLoadSuperTask(
      this.id,
      this.group,
      this.label,
      this.amplitudeInOffStatus,
      Nil,
      this.computeAmplitudePerSlotWithRestValueOnly
    )
    subTasksCopy.foreach(_.superTask = superTaskCopy)
    superTaskCopy.aggregatees = subTasksCopy
    superTaskCopy
  }

  ensureCorrectCreation()

}


