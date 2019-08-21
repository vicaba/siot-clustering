package test.load

import collection.CollecctionHelper.{mutableSetOf, orderedMutableSetOf}
import types.ops.SetOps._

import scala.collection.mutable

/**
  *
  * This is a mutable class.
  *
  * @param positionInT
  * @param _loads this parameters is mutable.
  */
class AccumulatedLoad private (override val id: Int,
                                    override val positionInT: Int,
                                    private val _loads: mutable.Set[Load],
                                    override val label: String = "")
    extends Load {

  def copy(loads: Set[Load] = this._loads.toSet): AccumulatedLoad =
    AccumulatedLoad(id, positionInT, mutableSetOf(Load.deepCopy(loads)))

  def loads: Set[Load] = _loads.toSet

  def flexibleLoads: Set[FlexibleLoad] =
    loads.filter(_.isInstanceOf[FlexibleLoad]).asInstanceOf[Set[FlexibleLoad]]

  def fixedLoads: Set[FixedLoad] =
    loads.filter(_.isInstanceOf[FixedLoad]).asInstanceOf[Set[FixedLoad]]

  def accumulatedLoads: Set[AccumulatedLoad] =
    loads.filter(_.isInstanceOf[AccumulatedLoad]).asInstanceOf[Set[AccumulatedLoad]]

  // Todo: changed
  override def span: Int = Load.span(loads)

  override def amplitudePerSlot: Vector[Double] = Load.amplitudePerSlot(loads)

  override def totalEnergy: Double =
    loads.toList.map(_.totalEnergy).foldLeft(0.0)((accum, l) => accum + l)

  override def toString: String = s"Acc($amplitudePerSlot)"

  def +=(y: Load): AccumulatedLoad = {
    this._loads += y
    this
  }

  def ++=(y: Iterable[Load]): AccumulatedLoad = {
    this._loads ++= y
    this
  }

  def -=(y: Load): AccumulatedLoad = {
    this._loads -= y
    this
  }

  def --=(y: Iterable[Load]): AccumulatedLoad = {
    this._loads --= y
    this
  }

  def -/+=(y: Load): AccumulatedLoad = {
    this._loads -/+= y
    this
  }

}

object AccumulatedLoad {

  def apply(id: Int, positionInT: Int, load: Load): AccumulatedLoad =
    new AccumulatedLoad(id, positionInT, new scala.collection.mutable.HashSet[Load]() += load)

  def apply(id: Int, positionInT: Int, loads: Traversable[Load]): AccumulatedLoad =
    new AccumulatedLoad(id, positionInT, mutableSetOf(loads))

  def keepLoadOrder(id: Int, positionInT: Int, loads: Traversable[Load]): AccumulatedLoad =
    new AccumulatedLoad(id, positionInT, orderedMutableSetOf(loads))

}
