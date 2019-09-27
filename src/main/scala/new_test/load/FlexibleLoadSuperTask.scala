package new_test.load

import breeze.linalg.DenseVector
import new_test.load.Load.{GroupId, LoadId}
import types.clusterer.DataTypeMetadata

object FlexibleLoadSuperTask {

  def apply(
    id: LoadId,
    group: GroupId,
    label: String,
    amplitudeInOffStatus: Double,
    _aggregatees: List[FlexibleLoadSubTask],
    _computeAmplitudePerSlotWithRestValueOnly: Boolean)
    (implicit amplitudePerSlotMetadata: DataTypeMetadata
    ): FlexibleLoadSuperTask =
    new FlexibleLoadSuperTask(id, group, label, amplitudeInOffStatus, _aggregatees, _computeAmplitudePerSlotWithRestValueOnly)

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

  ensureCorrectCreation()

  override def startPositionInTime: GroupId = 0

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

  override def amplitudePerSlot: DenseVector[Double] = ???

}


