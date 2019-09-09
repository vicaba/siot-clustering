package test.load

import test.load.Load.LoadId
import test.{SequenceSplitStrategy, load}

object FlexibleLoadTask {

  def buildFromSuperTask(spanSlotFlexibleLoadSuperTask: FlexibleLoadSuperTask): FlexibleLoad = {
    Load.amplitudePerSlot(spanSlotFlexibleLoadSuperTask.aggregatees)
    FlexibleLoad(
      spanSlotFlexibleLoadSuperTask.id,
      spanSlotFlexibleLoadSuperTask.positionInT,
      spanSlotFlexibleLoadSuperTask.amplitudePerSlot,
      spanSlotFlexibleLoadSuperTask.label
    )
  }

  def splitIntoSubTasks(flexibleLoad: FlexibleLoad,
    splitStrategy: SequenceSplitStrategy, loadIdCounter: Option[LoadId] = None): (FlexibleLoadSuperTask, LoadId) = {

    var loadId = loadIdCounter.getOrElse(0)

    val splitResults = splitStrategy(flexibleLoad.amplitudePerSlot)

    val superTask: FlexibleLoadSuperTask = load.FlexibleLoadSuperTask(
      loadId,
      0,
      null,
      flexibleLoad.span,
      splitResults.consecutiveValue,
      flexibleLoad.label
    )

    loadId = loadId + 1

    superTask.aggregatees = splitResults.results.map { result =>
      loadId = loadId + 1
      FlexibleLoadSubTask(superTask, loadId, result.index, result.seq.toVector, "subTask-" + flexibleLoad.label)
    }.toList

    (superTask, loadId)

  }

}

object FlexibleLoadSuperTask {
  def apply(id: Int,
    positionInT: Int,
    agregatees: List[FlexibleLoadSubTask],
    span: Int,
    restingValue: Double,
    label: String): FlexibleLoadSuperTask =
    new FlexibleLoadSuperTask(id, positionInT, agregatees, span, restingValue, label)
}

class FlexibleLoadSuperTask(override val id: Int,
  override val positionInT: Int,
  private var _aggregatees: List[FlexibleLoadSubTask],
  override val span: Int,
  val restValue: Double,
  override val label: String = "",
  private var _computeAmplitudePerSlotWithRestValueOnly: Boolean = false)
  extends Load {

  def areAggregateesOverlapped: Boolean = Load.areLoadsOverlapped(_aggregatees)

  def setComputeAmplitudePerSlotWithRestValueOnly(opt: Boolean): FlexibleLoadSuperTask = {
    this._computeAmplitudePerSlotWithRestValueOnly = opt
    this
  }

  def aggregatees_=(agg: List[FlexibleLoadSubTask]): FlexibleLoadSuperTask = {
    _aggregatees = agg
    this
  }

  def aggregatees: List[FlexibleLoadSubTask] = _aggregatees

  def computeAmplitudePerSlotWithRestValueOnly: Boolean = this._computeAmplitudePerSlotWithRestValueOnly

  //TODO: Assumes that start positionInT starts at 0. See Load.amplitudePerSlotEnforceSpan
  override def amplitudePerSlot: Vector[Double] =
    if (computeAmplitudePerSlotWithRestValueOnly) {
      Load.amplitudePerSlotEnforceSpan(aggregatees.map(_.copyWithAmplitudePerSlotToZero()), span, restValue)
    } else {
      Load.amplitudePerSlotEnforceSpan(aggregatees, span, restValue)
    }

  def toSpanSlotFlexibleLoad: FlexibleLoad = FlexibleLoadTask.buildFromSuperTask(this)

  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)

  def copy(id: Int = this.id,
    positionInT: Int = this.positionInT,
    agregatees: List[FlexibleLoadSubTask] = this.aggregatees,
    span: Int = this.span,
    restValue: Double = this.restValue,
    label: String = this.label): FlexibleLoadSuperTask = {
    val newSuperTask: FlexibleLoadSuperTask = new FlexibleLoadSuperTask(id, positionInT, null, span, restValue, label, computeAmplitudePerSlotWithRestValueOnly)
    newSuperTask.aggregatees_=(agregatees.map(_.copyWithNewSuperTask(newSuperTask)))
    newSuperTask
  }

  override def toString: String = s"FlexibleLoadSuperTask($id)"

}

object FlexibleLoadSubTask {
  def apply(superTask: FlexibleLoadSuperTask,
    id: Int,
    positionInT: Int,
    amplitudePerSlot: Vector[Double],
    label: String): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(superTask, id, positionInT, amplitudePerSlot, label)
}

class FlexibleLoadSubTask private(private var _superTask: FlexibleLoadSuperTask,
  override val id: Int,
  private val _positionInT: Int,
  override val amplitudePerSlot: Vector[Double],
  override val label: String = "")
  extends FlexibleLoad(id, _positionInT, amplitudePerSlot, label) {

  override def positionInT: Int = super.positionInT

  def superTask_=(superTask: FlexibleLoadSuperTask): FlexibleLoadSubTask = {
    _superTask = superTask
    this
  }

  def superTask: FlexibleLoadSuperTask = _superTask

  override def equals(obj: Any): Boolean = obj match {
    case task: FlexibleLoadSubTask =>
      task.superTask.id == this.superTask.id && super.equals(obj)
    case _ => super.equals(obj)
  }

  override def exactCopy(): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(this._superTask, this.id, this.positionInT, this.amplitudePerSlot, this.label)

  //TODO: Change that
  override def copy(id: Int = this.id,
    positionInT: Int = this.positionInT,
    amplitudePerSlot: Vector[Double] = this.amplitudePerSlot,
    label: String = this.label): FlexibleLoad =
    new FlexibleLoadSubTask(this._superTask,
      id,
      positionInT,
      amplitudePerSlot,
      label)

  def copyWithAmplitudePerSlotToZero(): FlexibleLoadSubTask = {
    new FlexibleLoadSubTask(this._superTask,
      this.id,
      this.positionInT,
      Vector.fill(this.amplitudePerSlot.size)(0.0),
      this.label)
  }

  def copyWithNewSuperTask(_superTask: FlexibleLoadSuperTask = this.superTask): FlexibleLoadSubTask = {
    new FlexibleLoadSubTask(_superTask, this.id, this.positionInT, this.amplitudePerSlot, this.label)
  }

  override def toString: String = s"FlexibleLoadSubTask($id)"

}
