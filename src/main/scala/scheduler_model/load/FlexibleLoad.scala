package scheduler_model.load

import Load._
import breeze.linalg._
import scheduler_model.sequence_split.SequenceSplitStrategy
import types.clusterer.DataTypeMetadata

object FlexibleLoad {
  def apply
  (
    id: LoadId, group: GroupId, label: String, startPositionInTime: Int, amplitudePerSlot: Vector[Double]
  ): FlexibleLoad =
    new FlexibleLoad(id, group, label, startPositionInTime, amplitudePerSlot)

  def splitIntoSubTasks(flexibleLoad: FlexibleLoad,
    splitStrategy: SequenceSplitStrategy, loadIdCounter: Option[LoadId] = None): (FlexibleLoadSuperTask, LoadId) = {

    var loadId = loadIdCounter.getOrElse(0)

    val splitResults = splitStrategy(flexibleLoad.amplitudePerSlot.toDenseVector.toScalaVector())

    val superTask: FlexibleLoadSuperTask = FlexibleLoadSuperTask(
      flexibleLoad.id,
      flexibleLoad.group,
      flexibleLoad.label,
      splitResults.consecutiveValue,
      Nil,
      computeAmplitudePerSlotWithRestValueOnly = false
    )(flexibleLoad.amplitudePerSlotMetadata)

    loadId = loadId + 1

    superTask.aggregatees = splitResults.results.map { result =>
      loadId = loadId + 1
      FlexibleLoadSubTask(
        loadId,
        superTask.id,
        flexibleLoad.label + " / SubTask(" + loadId + "), SuperTask(id=" + superTask.id + ",group=" + superTask.group + ")",
        result.index,
        DenseVector[Double](result.seq.toVector: _*),
        superTask)
    }.toList

    (superTask, loadId)

  }
}

class FlexibleLoad(
  override val id: LoadId,
  override val group: GroupId,
  override val label: String,
  protected val _startPositionInTime: Int,
  override val amplitudePerSlot: Vector[Double]
)
  extends SingleLoad {

  protected var __startPositionInTime: Int = _startPositionInTime

  override val amplitudePerSlotMetadata: DataTypeMetadata = DataTypeMetadata.generateDataTypeMetadata(forColumns = amplitudePerSlot.length)

  def startPositionInTime_=(pos: Int): FlexibleLoad = {
    __startPositionInTime = pos
    this
  }

  override def startPositionInTime: Int = __startPositionInTime

  def copy(): FlexibleLoad = FlexibleLoad(id, group, label, startPositionInTime, amplitudePerSlot)

  //ensureCorrectCreation()

}
