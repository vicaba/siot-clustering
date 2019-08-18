package test.load

import test.{SequenceSplitStrategy, load}

object FlexibleLoadTask {

  def buildFromSuperTask(spanSlotFlexibleLoadSuperTask: FlexibleLoadSuperTask): FlexibleLoad = {
    Load.amplitudePerSlot(spanSlotFlexibleLoadSuperTask.agregatees)
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

    lazy val superTask: FlexibleLoadSuperTask = load.FlexibleLoadSuperTask(
      0,
      0, {
        splitResults.results.zipWithIndex.map {
          case (result, idx) =>
            FlexibleLoadSubTask(superTask, idx, result.index, result.seq.toVector, "subTask-" + flexibleLoad.label)
        }.toList
      },
      flexibleLoad.span,
      splitResults.consecutiveValue,
      flexibleLoad.label
    )
    superTask

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
                            val agregatees: List[FlexibleLoadSubTask],
                            override val span: Int,
                            val restValue: Double,
                            override val label: String = "")
    extends Load {

  override def amplitudePerSlot: Vector[Double] = Load.amplitudePerSlotEnforceSpan(agregatees, span, restValue)

  def toSpanSlotFlexibleLoad: FlexibleLoad = FlexibleLoadTask.buildFromSuperTask(this)

  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)

  def copy(id: Int = this.id,
           positionInT: Int = this.positionInT,
           agregatees: List[FlexibleLoadSubTask] = this.agregatees,
           span: Int = this.span,
           restValue: Double = this.restValue,
           label: String = this.label): FlexibleLoadSuperTask = {
    lazy val newSuperTask: FlexibleLoadSuperTask = new FlexibleLoadSuperTask(id,
                              positionInT,
                              agregatees.map(_.copyWithNewSuperTask(_superTask = newSuperTask)),
                              span,
                              restValue,
                              label)
    newSuperTask
  }

}

object FlexibleLoadSubTask {
  def apply(superTask: => FlexibleLoadSuperTask,
            id: Int,
            positionInT: Int,
            amplitudePerSlot: Vector[Double],
            label: String): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(superTask, id, positionInT, amplitudePerSlot, label)
}

class FlexibleLoadSubTask private (_superTask: => FlexibleLoadSuperTask,
                                   override val id: Int,
                                   private var _positionInT: Int,
                                   override val amplitudePerSlot: Vector[Double],
                                   override val label: String = "")
    extends FlexibleLoad(id, _positionInT, amplitudePerSlot, label) {

  def superTask: FlexibleLoadSuperTask = _superTask

  override def equals(obj: Any): Boolean = obj match {
    case task: FlexibleLoadSubTask =>
      task.superTask.id == this.superTask.id && super.equals(obj)
    case _ => super.equals(obj)
  }

  override def exactCopy(): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(this._superTask, this.id, this.positionInT, this.amplitudePerSlot, this.label)

  def copyWithNewSuperTask(_superTask: => FlexibleLoadSuperTask = this.superTask): FlexibleLoadSubTask = {
    new FlexibleLoadSubTask(_superTask, this.id, this.positionInT, this.amplitudePerSlot, this.label)
  }
}
