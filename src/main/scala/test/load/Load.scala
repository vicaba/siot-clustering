package test.load

import algebra.SeqOps
import breeze.linalg.DenseVector
import collection.CollecctionHelper._
import metrics.DenseVectorReprOps
import test.{SequenceSplitStrategy, load}
import types.ops.SetOps._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.util.Try

trait Load {

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
  def amplitudePerSlot: Vector[Double]
  def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, l) => accum + l)
  def span: Int = amplitudePerSlot.size
  def peak: Double = amplitudePerSlot.max
}

trait SingleLoad extends Load

trait FlexibleLoadT extends SingleLoad

object Load {

  //def deepCopy(loads: Traversable[Load]): Traversable[Load] = loads.map(deepCopy)

  def deepCopy[L <: Load](loads: Traversable[L]): Traversable[L] = loads.map(deepCopyOne)

  def deepCopyOne[L <: Load](load: L): L = {
    load match {
      case l: AccumulatedLoad       => l.copy()
      case l: FixedLoad             => l.copy()
      case l: FlexibleLoad          => l.exactCopy()
      case l: FlexibleLoadSuperTask => l.copy()
      case l: FlexibleLoadSubTask   => l.exactCopy()
    }
  }.asInstanceOf[L]

  def toSpanSlotFixedLoad(s: Seq[Double]): FixedLoad = {
    FixedLoad(0, 0, s.toVector)
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
    if (loads.isEmpty) Vector.empty[Double]
    else
      SeqOps.sum(
        loads.toList
          .map(Load.expandSpanSlotLoadToVector(_, Load.span(loads)))
      )

  def amplitudePerSlotEnforceSpan(loads: Traversable[Load], span: Int, restValue: Double = Double.NaN): Vector[Double] =
    if (loads.isEmpty) Vector.fill(span)(restValue)
    else {
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

      val sumWithRestPositionsAtRestValue = sum.zip(restPositions).map {
        case (s, r) =>
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
