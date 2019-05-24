package test

import breeze.linalg.DenseVector
import metrics.DenseVectorReprOps

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.language.higherKinds

sealed trait Load {

  /**
    * Indicates the slot time when this load starts
    */
  def positionInT: Int
  def totalEnergy: Double
}

sealed trait IdLoad extends Load {

  def id: Int

  override def equals(obj: Any): Boolean = obj match {
    case s: IdLoad => s.id == this.id
    case _         => false
  }

  override def hashCode(): Int = this.id

}

sealed trait SingleLoad extends IdLoad

sealed trait AccumulatedLoad extends IdLoad

sealed trait OneSlotLoad extends IdLoad

sealed trait SpanSlotLoad extends IdLoad {
  def span: Int
  def amplitudePerSlot: Vector[Double]
}

case class FixedLoad(override val id: Int, override val positionInT: Int, override val totalEnergy: Double)
    extends OneSlotLoad
    with SingleLoad {
  override def toString: String = s"Fi($id, $positionInT, $totalEnergy)"
}

case class OneSlotFlexibleLoad(override val id: Int, override val positionInT: Int, override val totalEnergy: Double)
    extends OneSlotLoad
    with SingleLoad {
  override def toString: String = s"Fl($id, $positionInT, $totalEnergy)"
}

case class OneSlotAccumulatedLoad(override val positionInT: Int, loads: List[OneSlotLoad])
    extends OneSlotLoad
    with AccumulatedLoad {
  override val id: Int          = positionInT
  def totalEnergy: Double       = loads.foldLeft(0.0)((accum, load) => accum + load.totalEnergy)
  override def toString: String = s"Acc($positionInT, $totalEnergy -> $loads)"
}

case class SpanSlotFlexibleLoad(override val id: Int,
                                override val positionInT: Int,
                                override val span: Int,
                                override val amplitudePerSlot: Vector[Double])
    extends SpanSlotLoad
    with SingleLoad {
  override def totalEnergy: Double = amplitudePerSlot.foldLeft(0.0)((accum, a) => accum + a)
}

case class SpanSlotAccumulatedLoad(override val positionInT: Int,
                                   fixedLoads: List[FixedLoad],
                                   private val loads: Set[SpanSlotFlexibleLoad])
    extends SpanSlotLoad
    with AccumulatedLoad {

  override val span: Int = fixedLoads.size

  override val id: Int = positionInT

  override val amplitudePerSlot: Vector[Double] = loads
    .map(expandSpanSlotFlexibleLoadToVector(_, fixedLoads.size))
    .toIterator
    .foldLeft(fixedLoads.map(_.totalEnergy).toVector) { (itrA, itrB) =>
      itrA.zip(itrB).map {
        case (a, b) =>
          a + b
      }
    }


  def totalEnergy: Double =
    (fixedLoads.map(_.totalEnergy) ++: loads.map(_.totalEnergy)).foldLeft(0.0)((accum, l) => accum + l)

  override def toString: String = s"Acc($positionInT, $totalEnergy -> $loads)"

  private def expandSpanSlotFlexibleLoadToVector(load: SpanSlotFlexibleLoad, vectorSize: Int): Vector[Double] = {
    ((for (_ <- 0 until load.positionInT) yield 0.0) ++ load.amplitudePerSlot ++ (for (_ <- span until vectorSize)
      yield 0.0)).toVector
  }

}

class Loads(val fixedLoads: Vector[FixedLoad], val flexibleLoads: Vector[OneSlotFlexibleLoad])

object Load {

  def toFixedLoads[S[X] <: Seq[X]](s: S[Double])(
      implicit cbf: CanBuildFrom[Nothing, FixedLoad, S[FixedLoad]]): S[FixedLoad] = {
    s.zipWithIndex.map { case (e, idx) => FixedLoad(idx, idx - 1, e) }.to[S]
  }

  def toFlexibleLoads[S[X] <: Seq[X]](s: S[Double])(
      implicit cbf: CanBuildFrom[Nothing, OneSlotFlexibleLoad, S[OneSlotFlexibleLoad]]): S[OneSlotFlexibleLoad] = {
    s.zipWithIndex.map { case (e, idx) => OneSlotFlexibleLoad(idx, idx - 1, e) }.to[S]
  }

  def flatten(s: Seq[Load]): Vector[IdLoad] =
    s.flatMap {
      case al: OneSlotAccumulatedLoad => al.loads
      case s: IdLoad                  => Vector(s)
      case _                          => Vector()
    }.toVector

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
