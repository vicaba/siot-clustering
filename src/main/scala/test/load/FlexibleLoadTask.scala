package test.load

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
                        splitStrategy: SequenceSplitStrategy[Double]): FlexibleLoadSuperTask = {

    val splitResults = splitStrategy(flexibleLoad.amplitudePerSlot)

    val superTask: FlexibleLoadSuperTask = load.FlexibleLoadSuperTask(
      0,
      0,
      null,
      flexibleLoad.span,
      splitResults.consecutiveValue,
      flexibleLoad.label
    )

    superTask.aggregatees = splitResults.results.zipWithIndex.map {
      case (result, idx) =>
        FlexibleLoadSubTask(superTask, idx, result.index, result.seq.toVector, "subTask-" + flexibleLoad.label)
    }.toList

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
                            override val label: String = "")
    extends Load {

  private var _computeAmplitudePerSlotWithRestValueOnly: Boolean = false

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
    val newSuperTask: FlexibleLoadSuperTask = new FlexibleLoadSuperTask(id, positionInT, null, span, restValue, label)
    newSuperTask.aggregatees_=(agregatees.map(_.copyWithNewSuperTask(newSuperTask)))
    newSuperTask
  }

}

object FlexibleLoadSubTask {
  def apply(superTask: FlexibleLoadSuperTask,
            id: Int,
            positionInT: Int,
            amplitudePerSlot: Vector[Double],
            label: String): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(superTask, id, positionInT, amplitudePerSlot, label)
}

class FlexibleLoadSubTask private (private var _superTask: FlexibleLoadSuperTask,
                                   override val id: Int,
                                   private val _positionInT: Int,
                                   override val amplitudePerSlot: Vector[Double],
                                   override val label: String = "")
    extends FlexibleLoad(id, _positionInT, amplitudePerSlot, label) {

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
}
