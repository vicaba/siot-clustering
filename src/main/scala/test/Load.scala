package test

import algebra.SeqOps
import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.ops.SetOps._

import scala.collection.mutable
import scala.language.higherKinds
import collection.CollecctionHelper._

import scala.util.Try

sealed trait Load {

  def id: Int

  val label = ""

  override def equals(obj: Any): Boolean = obj match {
    case s: Load => this.getClass == s.getClass && s.id == this.id && s.label == this.label
    case _       => false
  }

  override def hashCode(): Int = this.id

  /**
    * Indicates the slot time when this load starts
    */
  def positionInT: Int
  def totalEnergy: Double
  def span: Int
  def amplitudePerSlot: Vector[Double]
  def peak: Double = amplitudePerSlot.max
}

sealed trait SingleLoad extends Load

sealed trait AccumulatedLoad extends Load

case class SpanSlotFixedLoad(override val id: Int,
                             override val positionInT: Int,
                             override val amplitudePerSlot: Vector[Double],
                             override val label: String = "")
    extends SingleLoad {
  override def span: Int           = amplitudePerSlot.size
  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)
}

object SpanSlotFlexibleLoad {

  def apply(id: Int, positionInT: Int, amplitudePerSlot: Vector[Double], label: String = ""): SpanSlotFlexibleLoad =
    new SpanSlotFlexibleLoad(id, positionInT, amplitudePerSlot, label)

}

class SpanSlotFlexibleLoad(override val id: Int,
                           override val positionInT: Int,
                           override val amplitudePerSlot: Vector[Double],
                           override val label: String = "")
    extends SingleLoad {
  override val span: Int           = amplitudePerSlot.size
  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)
  def copy(id: Int = this.id,
           positionInT: Int = this.positionInT,
           amplitudePerSlot: Vector[Double] = this.amplitudePerSlot,
           label: String = this.label): SpanSlotFlexibleLoad =
    SpanSlotFlexibleLoad(id, positionInT, amplitudePerSlot, label)

}

object SpanSlotFlexibleLoadAggregate {

  def buildMemory(original: SpanSlotFlexibleLoad,
                  offValue: Double,
                  aggregatees: Seq[SpanSlotFlexibleLoad]): SpanSlotFlexibleLoadAggregateMemory =
    new SpanSlotFlexibleLoadAggregateMemory(
      OriginalSpanSlotFlexibleLoadData(original),
      offValue,
      aggregatees.map(agregatee => new AggregateeData(agregatee.id, agregatee.label)))

  object OriginalSpanSlotFlexibleLoadData {
    def apply(load: SpanSlotFlexibleLoad): OriginalSpanSlotFlexibleLoadData =
      new OriginalSpanSlotFlexibleLoadData(load.id, load.positionInT, load.amplitudePerSlot, load.label)
  }

  class OriginalSpanSlotFlexibleLoadData(val id: Int,
                                         val positionInT: Int,
                                         val amplitudePerSlot: Vector[Double],
                                         val label: String = "")

  private class AggregateeData(val id: Int, val label: String)

  class SpanSlotFlexibleLoadAggregateMemory(
      val originalData: SpanSlotFlexibleLoadAggregate.OriginalSpanSlotFlexibleLoadData,
      val offValue: Double,
      private val aggregatees: Seq[AggregateeData]) {
    assert(!aggregatees.exists(_.label != originalData.label),
           "At least one of the aggregatees does not have the same label as the original flexible load")
/*    def merge(allPossibleAggregatees: Seq[SpanSlotFlexibleLoad]): (SpanSlotFlexibleLoad, Seq[SpanSlotFlexibleLoad]) = {
      allPossibleAggregatees.filter(_.label == originalData.label)
      val newAggregatees = for {
        agg     <- allPossibleAggregatees.filter(_.label == originalData.label)
        aggData <- aggregatees
        if agg.id == aggData.id
      } yield agg

      val maxPosIntT = new

      for (i <- )

    }*/
  }

}

/**
  *
  * This is a mutable class.
  *
  * @param positionInT
  * @param _loads this parameters is mutable.
  */
case class SpanSlotAccumulatedLoad private (override val id: Int,
                                            override val positionInT: Int,
                                            private val _loads: mutable.Set[Load],
                                            override val label: String = "")
    extends AccumulatedLoad {

  def copy(loads: Set[Load] = this._loads.toSet): SpanSlotAccumulatedLoad =
    SpanSlotAccumulatedLoad(id, positionInT, mutableSetOf(loads))

  def loads: Set[Load] = _loads.toSet

  def flexibleLoads: Set[SpanSlotFlexibleLoad] =
    loads.filter(_.isInstanceOf[SpanSlotFlexibleLoad]).asInstanceOf[Set[SpanSlotFlexibleLoad]]

  def fixedLoads: Set[SpanSlotFixedLoad] =
    loads.filter(_.isInstanceOf[SpanSlotFixedLoad]).asInstanceOf[Set[SpanSlotFixedLoad]]

  def accumulatedLoads: Set[SpanSlotAccumulatedLoad] =
    loads.filter(_.isInstanceOf[SpanSlotAccumulatedLoad]).asInstanceOf[Set[SpanSlotAccumulatedLoad]]

  // Todo: changed
  override def span: Int = Load.span(loads)

  override def amplitudePerSlot: Vector[Double] =
    SeqOps.sum(
      loads.toList
        .map(expandSpanSlotLoadToVector(_, span))
    )

  def totalEnergy: Double =
    loads.toList.map(_.totalEnergy).foldLeft(0.0)((accum, l) => accum + l)

  override def toString: String = s"Acc($positionInT, $totalEnergy -> ${_loads})"

  private def expandSpanSlotLoadToVector(load: Load, vectorSize: Int): Vector[Double] =
    (
      (for (_ <- 0 until load.positionInT) yield 0.0) ++
        load.amplitudePerSlot ++
        (for (_ <- (load.positionInT + load.span) until vectorSize) yield 0.0)
    ).toVector

  def +=(y: Load): SpanSlotAccumulatedLoad = {
    this._loads += y
    this
  }

  def ++=(y: Iterable[Load]): SpanSlotAccumulatedLoad = {
    this._loads ++= y
    this
  }

  def -=(y: Load): SpanSlotAccumulatedLoad = {
    this._loads -= y
    this
  }

  def --=(y: Iterable[Load]): SpanSlotAccumulatedLoad = {
    this._loads --= y
    this
  }

  def -/+=(y: Load): SpanSlotAccumulatedLoad = {
    this._loads -/+= y
    this
  }

}

object SpanSlotAccumulatedLoad {

  def apply(id: Int, positionInT: Int, load: Load): SpanSlotAccumulatedLoad =
    new SpanSlotAccumulatedLoad(id, positionInT, new scala.collection.mutable.HashSet[Load]() += load)

  def apply(id: Int, positionInT: Int, loads: Traversable[Load]): SpanSlotAccumulatedLoad =
    new SpanSlotAccumulatedLoad(id, positionInT, mutableSetOf(loads))

  def keepLoadOrder(id: Int, positionInT: Int, loads: Traversable[Load]): SpanSlotAccumulatedLoad =
    new SpanSlotAccumulatedLoad(id, positionInT, orderedMutableSetOf(loads))

}

object Load {

  def toSpanSlotFixedLoad(s: Seq[Double]): SpanSlotFixedLoad = {
    SpanSlotFixedLoad(0, 0, s.toVector)
  }

  def span(loads: Traversable[Load]): Int = Try(loads.map(l => l.span + l.positionInT).max - loads.map(_.positionInT).min).getOrElse(0)

  class LoadOrdering extends Ordering[Load] {
    override def compare(x: Load, y: Load): Int = implicitly[Ordering[Double]].compare(x.totalEnergy, y.totalEnergy)
  }

  val loadOrderingByPositionInTime: Ordering[Load] =
    (x: Load, y: Load) => implicitly[Ordering[Int]].compare(x.positionInT, y.positionInT)

  implicit val loadOrderingByAmplitude: Ordering[Load] =
    (x: Load, y: Load) => implicitly[Ordering[Double]].compare(x.totalEnergy, y.totalEnergy)

  implicit def toVector[X <: Load]: DenseVectorReprOps[X] = new DenseVectorReprOps[X] {

    override def apply(t: X): DenseVector[Double] = DenseVector(t.amplitudePerSlot: _*)

    override def zero(t: X): DenseVector[Double] = DenseVector((for (_ <- 1 to t.span) yield 0.0): _*)

  }

}
