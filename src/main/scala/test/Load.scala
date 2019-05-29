package test

import algebra.SeqOps
import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps
import types.ops.SetOps._

import scala.collection.mutable
import scala.language.higherKinds

sealed trait Load {

  def id: Int

  override def equals(obj: Any): Boolean = obj match {
    case s: Load => s.id == this.id && this.getClass == s.getClass
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
}

sealed trait SingleLoad extends Load

sealed trait AccumulatedLoad extends Load

case class SpanSlotFixedLoad(override val id: Int,
                             override val positionInT: Int,
                             override val amplitudePerSlot: Vector[Double])
    extends SingleLoad {
  override def span: Int           = amplitudePerSlot.size
  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)
}

case class SpanSlotFlexibleLoad(override val id: Int,
                                override val positionInT: Int,
                                override val amplitudePerSlot: Vector[Double])
    extends SingleLoad {
  override val span: Int           = amplitudePerSlot.size
  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)
}

case class SpanSlotAccumulatedLoad(override val positionInT: Int, loads: mutable.Set[Load]) extends AccumulatedLoad {

  val flexibleLoads: Set[SpanSlotFlexibleLoad] =
    loads.filter(_.isInstanceOf[SpanSlotFlexibleLoad]).asInstanceOf[mutable.Set[SpanSlotFlexibleLoad]].toSet

  val fixedLoads: Set[SpanSlotFixedLoad] =
    loads.filter(_.isInstanceOf[SpanSlotFixedLoad]).asInstanceOf[mutable.Set[SpanSlotFixedLoad]].toSet

  val accumulatedLoads: Set[SpanSlotAccumulatedLoad] =
    loads.filter(_.isInstanceOf[SpanSlotAccumulatedLoad]).asInstanceOf[mutable.Set[SpanSlotAccumulatedLoad]].toSet

  override val id: Int = positionInT

  override val span: Int = loads.map(l => l.span + l.positionInT).max

  override val amplitudePerSlot: Vector[Double] =
    SeqOps.sum(
      loads
        .map(expandSpanSlotLoadToVector(_, span))
        .toList
    )

  def totalEnergy: Double =
    (fixedLoads.map(_.totalEnergy) ++: loads.map(_.totalEnergy)).foldLeft(0.0)((accum, l) => accum + l)

  override def toString: String = s"Acc($positionInT, $totalEnergy -> $loads)"

  private def expandSpanSlotLoadToVector(load: Load, vectorSize: Int): Vector[Double] =
    (
      (for (_ <- 0 until load.positionInT) yield 0.0) ++
        load.amplitudePerSlot ++
        (for (_ <- (load.positionInT + load.span) until vectorSize) yield 0.0)
    ).toVector

}

object SpanSlotAccumulatedLoad {
  def apply(positionInT: Int, load: Load): SpanSlotAccumulatedLoad =
    new SpanSlotAccumulatedLoad(positionInT, new scala.collection.mutable.HashSet[Load]() += load)
}

object Load {

  def toSpanSlotFixedLoad(s: Seq[Double]): SpanSlotFixedLoad = {
    SpanSlotFixedLoad(0, 0, s.toVector)
  }

  class LoadOrdering extends Ordering[Load] {
    override def compare(x: Load, y: Load): Int = implicitly[Ordering[Double]].compare(x.totalEnergy, y.totalEnergy)
  }

  val loadOrderingByPositionInTime: Ordering[Load] =
    (x: Load, y: Load) => implicitly[Ordering[Int]].compare(x.positionInT, y.positionInT)

  implicit val loadOrderingByAmplitude: Ordering[Load] =
    (x: Load, y: Load) => implicitly[Ordering[Double]].compare(x.totalEnergy, y.totalEnergy)

  implicit def toVector[X <: Load]: DenseVectorReprOps[Vector[X]] = new DenseVectorReprOps[Vector[X]] {

    override def apply(t: Vector[X]): DenseVector[Double] = DenseVector(t.map(_.totalEnergy): _*)

    override def zero(t: Vector[X]): DenseVector[Double] = DenseVector((for (_ <- 1 to t.size) yield 0.0): _*)

  }

  object LoadOps {

    def +(x: SpanSlotAccumulatedLoad, y: Load): Load = {
      x.loads += y
      x
    }

    def -(x: SpanSlotAccumulatedLoad, y: Load): Load = {
      x.loads -= y
      x
    }

    def -/+(x: SpanSlotAccumulatedLoad, y: Load): Load = {
      x.loads -/+= y
      x
    }

  }

}
