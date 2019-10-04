package scheduler_model.load

import breeze.linalg._
import scheduler_model.load.Load.{GroupId, LoadId}
import types.clusterer.DataTypeMetadata
import collection.CollecctionHelper._
import scheduler_model.load.AccumulatedLoad
import scheduler_model.sequence_split.SequenceSplitStrategy
import types.ops.SetOps._

import scala.collection.mutable

object AccumulatedLoad {

  def apply(id: LoadId, group: GroupId, label: String, load: Load)(
      implicit amplitudePerSlotMetadata: DataTypeMetadata): AccumulatedLoad =
    new AccumulatedLoad(id, group, label, new scala.collection.mutable.HashSet[Load]() += load)

  def apply(id: LoadId, group: GroupId, label: String, loads: Traversable[Load])(
      implicit amplitudePerSlotMetadata: DataTypeMetadata): AccumulatedLoad =
    new AccumulatedLoad(id, group, label, mutableSetOf(loads))

  def keepLoadOrder(id: LoadId, group: GroupId, label: String, loads: Traversable[Load])(
      implicit amplitudePerSlotMetadata: DataTypeMetadata): AccumulatedLoad =
    new AccumulatedLoad(id, group, label, orderedMutableSetOf(loads))

  object AutoSpanFromLoads {

    def apply(id: LoadId, group: GroupId, label: String, load: Load): AccumulatedLoad =
      new AccumulatedLoad(id, group, label, new scala.collection.mutable.HashSet[Load]() += load)(
        DataTypeMetadata.generateDataTypeMetadata(forColumns = load.span))

    def apply(id: LoadId, group: GroupId, label: String, loads: Traversable[Load]): AccumulatedLoad =
      new AccumulatedLoad(id, group, label, mutableSetOf(loads))(
        DataTypeMetadata.generateDataTypeMetadata(forColumns = loads.map(_.span).max))

    def keepLoadOrder(id: LoadId, group: GroupId, label: String, loads: Traversable[Load]): AccumulatedLoad =
      new AccumulatedLoad(id, group, label, orderedMutableSetOf(loads))(
        DataTypeMetadata.generateDataTypeMetadata(forColumns = loads.map(_.span).max))

  }

  object Mutate {

    def splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(accLoad: AccumulatedLoad,
                                                                   splitStrategy: SequenceSplitStrategy,
                                                                   idC: Option[Int] = None): AccumulatedLoad = {

      var _idC = idC.getOrElse(0)

      accLoad.flexibleLoads.foreach { fl =>
        val (loadTask, lastUsedLoadId) = FlexibleLoad.splitIntoSubTasks(fl, splitStrategy, Some(_idC))
        _idC = lastUsedLoadId + 1

        val flexibleLoadsToRemove = List(fl)
        val flexibleLoadsToAdd    = List(loadTask.computeAmplitudePerSlotWithRestValueOnly = true) ++ loadTask.aggregatees

        accLoad --= flexibleLoadsToRemove
        accLoad ++= flexibleLoadsToAdd
      }
      accLoad
    }

  }

}

class AccumulatedLoad(
    override val id: LoadId,
    override val group: GroupId,
    override val label: String,
    val loads: mutable.Set[Load]
)(implicit override val amplitudePerSlotMetadata: DataTypeMetadata)
    extends Load {

  def flexibleLoads: Set[FlexibleLoad] =
    loads.filter(_.isInstanceOf[FlexibleLoad]).toSet.asInstanceOf[Set[FlexibleLoad]]

  def fixedLoads: Set[FixedLoad] =
    loads.filter(_.isInstanceOf[FixedLoad]).toSet.asInstanceOf[Set[FixedLoad]]

  override def startPositionInTime: Int = loads.map(_.startPositionInTime).min

  override def amplitudePerSlot: Vector[Double] =
    if (loads.nonEmpty) sum(loads.toList.map(LoadOps.expandToCols(_, amplitudePerSlotMetadata.Columns)))
    else amplitudePerSlotMetadata.EmptySyntheticData()

  /**
    * Shorthand for .loads +=
    * @param y
    * @return
    */
  def +=(y: Load): AccumulatedLoad = {
    this.loads += y
    this
  }

  /**
    * Shorthand for .loads -=
    * @param y
    * @return
    */
  def -=(y: Load): AccumulatedLoad = {
    this.loads -= y
    this
  }

  /**
    * Shorthand for .loads ++=
    * @param y
    * @return
    */
  def ++=(y: Iterable[Load]): AccumulatedLoad = {
    this.loads ++= y
    this
  }

  /**
    * Shorthand for .loads --=
    * @param y
    * @return
    */
  def --=(y: Iterable[Load]): AccumulatedLoad = {
    this.loads --= y
    this
  }

  /**
    * Shorthand for .loads -/+=
    * @param y
    * @return
    */
  def -/+=(y: Load): AccumulatedLoad = {
    this.loads -/+= y
    this
  }

  private[load] def copy(addSuperTaskSubTasks: Boolean): AccumulatedLoad =
    AccumulatedLoad(id, group, label, mutableSetOf(LoadOps.copy(loads, addSuperTaskSubTasks)))

}
