package test

import algebra.SeqOps
import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.ops.SetOps._

import scala.collection.mutable
import scala.language.higherKinds
import collection.CollecctionHelper._
import test.Load.span

import scala.collection.mutable.ListBuffer
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
  private var _positionInT: Int,
                           private val _amplitudePerSlot: Vector[Double],
                           override val label: String = "")
    extends SingleLoad {

  override def positionInT: Int = _positionInT

  def positionInT_=(pos: Int): SpanSlotFlexibleLoad = {
    _positionInT = pos
    this
  }

  override val span: Int = amplitudePerSlot.size

  override def amplitudePerSlot: Vector[Double] = _amplitudePerSlot

  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)

  def copy(id: Int = this.id,
           positionInT: Int = this.positionInT,
           amplitudePerSlot: Vector[Double] = this.amplitudePerSlot,
           label: String = this.label): SpanSlotFlexibleLoad =
    SpanSlotFlexibleLoad(id, positionInT, amplitudePerSlot, label)

}

object SpanSlotFlexibleLoadTask {

  def buildFromSuperTask(spanSlotFlexibleLoadSuperTask: SpanSlotFlexibleLoadSuperTask): SpanSlotFlexibleLoad = {
    Load.amplitudePerSlot(spanSlotFlexibleLoadSuperTask.agregatees)
    SpanSlotFlexibleLoad(
      spanSlotFlexibleLoadSuperTask.id,
      spanSlotFlexibleLoadSuperTask.positionInT,
      spanSlotFlexibleLoadSuperTask.amplitudePerSlot,
      spanSlotFlexibleLoadSuperTask.label
    )
  }

  def splitIntoSubTasks(spanSlotFlexibleLoad: SpanSlotFlexibleLoad,
                        splitStrategy: SequenceSplitStrategy[Double]): SpanSlotFlexibleLoadSuperTask = {

    val splitResults = splitStrategy(spanSlotFlexibleLoad.amplitudePerSlot)

    lazy val superTask: SpanSlotFlexibleLoadSuperTask = SpanSlotFlexibleLoadSuperTask(
      1,
      1, {
        splitResults.results.zipWithIndex.map {
          case (result, idx) =>
            SpanSlotFlexibleLoadSubTask(superTask,
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

object SpanSlotFlexibleLoadSuperTask {
  def apply(id: Int,
            positionInT: Int,
            agregatees: List[SpanSlotFlexibleLoadSubTask],
            span: Int,
            restingValue: Double,
            label: String): SpanSlotFlexibleLoadSuperTask =
    new SpanSlotFlexibleLoadSuperTask(id, positionInT, agregatees, span, restingValue, label)
}

case class SpanSlotFlexibleLoadSuperTask(override val id: Int,
                                         var _positionInT: Int,
                                         agregatees: List[SpanSlotFlexibleLoadSubTask],
                                         override val span: Int,
                                         restValue: Double,
                                         override val label: String = "")
    extends SpanSlotFlexibleLoad(id, _positionInT, Load.amplitudePerSlotEnforceSpan(agregatees, span, restValue), label) {

  override def amplitudePerSlot: Vector[Double] = Load.amplitudePerSlotEnforceSpan(agregatees, span, restValue)

  def toSpanSlotFlexibleLoad: SpanSlotFlexibleLoad = SpanSlotFlexibleLoadTask.buildFromSuperTask(this)

}

object SpanSlotFlexibleLoadSubTask {
  def apply(parentFlexibleLoad: => SpanSlotFlexibleLoad,
            id: Int,
            positionInT: Int,
            amplitudePerSlot: Vector[Double],
            label: String): SpanSlotFlexibleLoadSubTask =
    new SpanSlotFlexibleLoadSubTask(parentFlexibleLoad, id, positionInT, amplitudePerSlot, label)
}

class SpanSlotFlexibleLoadSubTask private (_parentFlexibleLoad: => SpanSlotFlexibleLoad,
                                           override val id: Int,
                                           var _positionInT: Int,
                                           override val amplitudePerSlot: Vector[Double],
                                           override val label: String = "")
    extends SpanSlotFlexibleLoad(id, _positionInT, amplitudePerSlot, label) {

  def parentFlexibleLoad: SpanSlotFlexibleLoad = _parentFlexibleLoad

  override def equals(obj: Any): Boolean = obj match {
    case task: SpanSlotFlexibleLoadSubTask =>
      task.parentFlexibleLoad.id == this.parentFlexibleLoad.id && super.equals(obj)
    case _ => super.equals(obj)
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

  override def amplitudePerSlot: Vector[Double] = Load.amplitudePerSlot(loads)

  def totalEnergy: Double =
    loads.toList.map(_.totalEnergy).foldLeft(0.0)((accum, l) => accum + l)

  override def toString: String = s"Acc($positionInT, $totalEnergy -> ${_loads})"

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

  def span(loads: Traversable[Load]): Int =
    Try(loads.map(l => l.span + l.positionInT).max - loads.map(_.positionInT).min).getOrElse(0)

  def expandSpanSlotLoadToVector(load: Load, vectorSize: Int): Vector[Double] =
    (
      (for (_ <- 0 until load.positionInT) yield 0.0) ++
        load.amplitudePerSlot ++
        (for (_ <- (load.positionInT + load.span) until vectorSize) yield 0.0)
    ).toVector

  def amplitudePerSlot(loads: Traversable[Load]): Vector[Double] =
    SeqOps.sum(
      loads.toList
        .map(Load.expandSpanSlotLoadToVector(_, Load.span(loads)))
    )

  def amplitudePerSlotEnforceSpan(loads: Traversable[Load], span: Int, restValue: Double = Double.NaN): Vector[Double] = {
    val sum = SeqOps.sum(
      loads.toList
        .map(Load.expandSpanSlotLoadToVector(_, span))
    )

    val restPositions = ListBuffer.fill(span)(true)

    loads.foreach { l =>
      for (idx <- l.positionInT until (l.positionInT + l.span)) {
        restPositions(idx) = false
      }
    }

    val sumWithRestPositionsAtRestValue = sum.zip(restPositions).map { case (s, r) =>
      if (r) restValue else s
    }

    sumWithRestPositionsAtRestValue

  }

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
