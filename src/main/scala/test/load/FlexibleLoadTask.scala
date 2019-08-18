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

  def splitIntoSubTasks(spanSlotFlexibleLoad: FlexibleLoad,
                        splitStrategy: SequenceSplitStrategy[Double]): FlexibleLoadSuperTask = {

    val splitResults = splitStrategy(spanSlotFlexibleLoad.amplitudePerSlot)

    lazy val superTask: FlexibleLoadSuperTask = load.FlexibleLoadSuperTask(
      1,
      1, {
        splitResults.results.zipWithIndex.map {
          case (result, idx) =>
            FlexibleLoadSubTask(superTask,
                                idx,
                                result.index,
                                result.seq.toVector,
                                "subTask-" + spanSlotFlexibleLoad.label)
        }.toList
      },
      spanSlotFlexibleLoad.span,
      splitResults.consecutiveValue,
      spanSlotFlexibleLoad.label
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

case class FlexibleLoadSuperTask(override val id: Int,
                                 var _positionInT: Int,
                                 agregatees: List[FlexibleLoadSubTask],
                                 override val span: Int,
                                 restValue: Double,
                                 override val label: String = "")
    extends FlexibleLoad(id, _positionInT, Load.amplitudePerSlotEnforceSpan(agregatees, span, restValue), label) {

  override def amplitudePerSlot: Vector[Double] = Load.amplitudePerSlotEnforceSpan(agregatees, span, restValue)

  def toSpanSlotFlexibleLoad: FlexibleLoad = FlexibleLoadTask.buildFromSuperTask(this)

}

object FlexibleLoadSubTask {
  def apply(parentFlexibleLoad: => FlexibleLoad,
            id: Int,
            positionInT: Int,
            amplitudePerSlot: Vector[Double],
            label: String): FlexibleLoadSubTask =
    new FlexibleLoadSubTask(parentFlexibleLoad, id, positionInT, amplitudePerSlot, label)
}

class FlexibleLoadSubTask private (_parentFlexibleLoad: => FlexibleLoad,
                                   override val id: Int,
                                   var _positionInT: Int,
                                   override val amplitudePerSlot: Vector[Double],
                                   override val label: String = "")
    extends FlexibleLoad(id, _positionInT, amplitudePerSlot, label) {

  def parentFlexibleLoad: FlexibleLoad = _parentFlexibleLoad

  override def equals(obj: Any): Boolean = obj match {
    case task: FlexibleLoadSubTask =>
      task.parentFlexibleLoad.id == this.parentFlexibleLoad.id && super.equals(obj)
    case _ => super.equals(obj)
  }
}
