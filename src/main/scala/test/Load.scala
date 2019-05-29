package test

import algebra.SeqOps
import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps

import scala.collection.generic.CanBuildFrom
import scala.collection.{AbstractSeq, immutable}
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
}

sealed trait SingleLoad extends Load

sealed trait AccumulatedLoad extends Load

sealed trait OneSlotLoad extends Load

sealed trait SpanSlotLoad extends Load {
  def span: Int
  def amplitudePerSlot: Vector[Double]
}

case class SpanSlotFixedLoad(override val id: Int,
                             override val positionInT: Int,
                             override val amplitudePerSlot: Vector[Double])
    extends SpanSlotLoad
    with SingleLoad {
  override def span: Int           = amplitudePerSlot.size
  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)
}

case class SpanSlotFlexibleLoad(override val id: Int,
                                override val positionInT: Int,
                                override val amplitudePerSlot: Vector[Double])
    extends SpanSlotLoad
    with SingleLoad {
  override val span: Int           = amplitudePerSlot.size
  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)
}

case class SpanSlotAccumulatedLoad(override val positionInT: Int, val loads: Set[SpanSlotLoad])
    extends SpanSlotLoad
    with AccumulatedLoad {


  val flexibleLoads: Set[SpanSlotFlexibleLoad] = loads.filter(_.isInstanceOf[SpanSlotFlexibleLoad]).asInstanceOf[Set[SpanSlotFlexibleLoad]]

  val fixedLoads: Set[SpanSlotFixedLoad] = loads.filter(_.isInstanceOf[SpanSlotFixedLoad]).asInstanceOf[Set[SpanSlotFixedLoad]]

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

  private def expandSpanSlotLoadToVector(load: SpanSlotLoad, vectorSize: Int): Vector[Double] = {
    val res = (
      (for (_ <- 0 until load.positionInT) yield 0.0) ++
        load.amplitudePerSlot ++
        (for (_ <- (load.positionInT + load.span) until vectorSize) yield 0.0)
    ).toVector

    res
  }

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

}
